#!/usr/bin/env bash
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

EXAMPLES_DIR="test/examples"

if [[ ! -d "$EXAMPLES_DIR" ]]; then
  echo "Diretorio nao encontrado: $EXAMPLES_DIR"
  exit 1
fi

if command -v cabal >/dev/null 2>&1; then
  COMPILER_CMD=(cabal run compiler --)
elif [[ -x "./compiler" ]]; then
  COMPILER_CMD=(./compiler)
else
  echo "Nem ./compiler nem cabal foram encontrados."
  echo "Compile antes com 'cabal install --installdir=. --overwrite-policy=always -v0' ou rode dentro do container."
  exit 1
fi

passes=0
fails=0

run_stage() {
  local stage="$1"
  local file="$2"

  LAST_OUTPUT="$("${COMPILER_CMD[@]}" "--${stage}" "$file" 2>&1)"
  return $?
}

has_main() {
  local file="$1"
  if command -v rg >/dev/null 2>&1; then
    rg -q '^[[:space:]]*func[[:space:]]+main[[:space:]]*\(' "$file"
  else
    grep -Eq '^[[:space:]]*func[[:space:]]+main[[:space:]]*\(' "$file"
  fi
}

expectation_for() {
  local base="$1"
  case "$base" in
    ex07_array_oob_runtime.sl) echo "runtime_error" ;;
    ex08_first_class_function_limit.sl) echo "semantic_error" ;;
    *) echo "success" ;;
  esac
}

for file in "$EXAMPLES_DIR"/*.sl; do
  base="$(basename "$file")"
  expected="$(expectation_for "$base")"

  echo
  echo "==> $base (esperado: $expected)"

  if ! run_stage lexer "$file" >/dev/null; then
    echo "  [FAIL] lexer"
    echo "  detalhe: $LAST_OUTPUT"
    ((fails++))
    continue
  fi

  if ! run_stage parser "$file" >/dev/null; then
    echo "  [FAIL] parser"
    echo "  detalhe: $LAST_OUTPUT"
    ((fails++))
    continue
  fi

  if ! run_stage pretty "$file" >/dev/null; then
    echo "  [FAIL] pretty"
    echo "  detalhe: $LAST_OUTPUT"
    ((fails++))
    continue
  fi

  if run_stage semantic "$file" >/dev/null; then
    semantic_ok=1
  else
    semantic_ok=0
  fi

  if [[ "$expected" == "semantic_error" ]]; then
    if [[ "$semantic_ok" -eq 0 ]]; then
      echo "  [PASS] falha semantica esperada"
      echo "  detalhe: $LAST_OUTPUT"
      ((passes++))
    else
      echo "  [FAIL] deveria falhar na semantica"
      ((fails++))
    fi
    continue
  fi

  if [[ "$semantic_ok" -eq 0 ]]; then
    echo "  [FAIL] semantica"
    echo "  detalhe: $LAST_OUTPUT"
    ((fails++))
    continue
  fi

  if ! has_main "$file"; then
    echo "  [PASS] semantica ok (sem main, interp ignorado)"
    ((passes++))
    continue
  fi

  if run_stage interp "$file" >/dev/null; then
    interp_ok=1
  else
    interp_ok=0
  fi

  if [[ "$expected" == "runtime_error" ]]; then
    if [[ "$interp_ok" -eq 0 ]]; then
      echo "  [PASS] erro de runtime esperado"
      echo "  detalhe: $LAST_OUTPUT"
      ((passes++))
    else
      echo "  [FAIL] deveria falhar em runtime"
      ((fails++))
    fi
  else
    if [[ "$interp_ok" -eq 1 ]]; then
      echo "  [PASS]"
      ((passes++))
    else
      echo "  [FAIL] interpretador"
      echo "  detalhe: $LAST_OUTPUT"
      ((fails++))
    fi
  fi
done

echo
echo "Resumo: $passes passou/passaram, $fails falhou/falharam."

if [[ "$fails" -gt 0 ]]; then
  exit 1
fi
