# Pipeline do Compilador SL

## 1. Pipeline completo
```text
Entrada (.sl)
  -> Lexer (Tokenizacao)
  -> Parser (Construcao de AST)
  -> Analise Semantica e Type Checker
  -> Interpretador (execucao)
  -> Saida (prints / erros)
```

## 2. Fase 1: Lexer
Arquivos:
- `workspace/src/Lexer.x` (fonte Alex)
- `workspace/src/Lexer.hs` (gerado)
- `workspace/src/Token.hs` (tokens)

Entrada: `String` do codigo fonte.  
Saida: `[Token]`.

Responsabilidades:
- reconhecer palavras-chave, identificadores, literais e operadores.
- registrar linha/coluna em `Token.Position`.
- gerar excecao para caractere invalido (`LexerException`).

Regras relevantes:
- comentarios `//...`
- tipos base (`int`, `float`, `bool`, `string`, `void`)
- `forall`, `new`, `++`, `->`, etc.

## 3. Fase 2: Parser
Arquivos:
- `workspace/src/Parser.y` (fonte Happy)
- `workspace/src/Parser.hs` (gerado)
- `workspace/src/Parser.info` (estados da gramatica)

Entrada: `[Token]`  
Saida: `AST.Program`

Responsabilidades:
- validar estrutura gramatical.
- aplicar precedencia/associatividade.
- construir AST com acoes semanticas da gramatica.

Aspectos importantes:
- suporta `forall` com lista de type params (`forall a b .`).
- `for` com update `i++` preservado como `EPostInc`.
- converte `a.size` para `EArraySize` no parser.

## 4. Fase 3: AST
Arquivo:
- `workspace/src/AST.hs`

Representa:
- declaracoes (`DeclFunc`, `DeclStruct`, `DeclVar`)
- statements (if/while/for/assign/return)
- expressoes (chamada, aritmetica, arrays, structs, `EArraySize`, `EPostInc`)
- tipos sintaticos (`Type`)

AST e o contrato entre frontend, semantica e interpretador.

## 5. Fase 4: Analise semantica e type checker
Arquivos:
- `workspace/src/Semantic/Analyzer.hs`
- `workspace/src/TypeChecker/Types.hs`
- `workspace/src/TypeChecker/Unify.hs`
- `workspace/src/Environment/*`
- `workspace/src/Errors/*`

Entrada: `Program`  
Saida: `Either [Diagnostic] CheckedProgram`

Checagens:
- declaracoes duplicadas (struct/func/var).
- escopo de variaveis.
- tipos em atribuicao, condicao e retorno.
- chamadas de funcao (aridade e tipos).
- structs e campos.
- arrays e indice inteiro.
- funcao como valor (tipos `TFun`/`STFun`).
- `for` com declaracao implicita de indice no init (quando necessario).

## 6. Fase 5: Interpretador
Arquivos:
- `workspace/src/Interpreter/Eval.hs`
- `workspace/src/Interpreter/Value.hs`
- `workspace/src/Interpreter/Builtins.hs`

Entrada: `CheckedProgram`  
Saida: `Either String [String]` (linhas de output)

Modelo:
- estado de runtime com pilha de escopos, funcoes, structs e buffer de saida.
- execucao de statements com propagacao de retorno.
- builtins (`print`).
- acesso e atribuicao em `LValue` com caminho (campo e indice).
- bounds check de array.

## 7. Fase 6: CLI
Arquivo:
- `workspace/app/Main.hs`

Fluxos:
- `--lexer`: apenas lexer.
- `--parser`: lexer + parser + arvore textual.
- `--pretty`: lexer + parser + formatacao.
- `--semantic`: lexer + parser + semantica.
- `--interp`: lexer + parser + semantica + interpretador.

## 8. Fluxo detalhado do --interp
```text
readFile
  -> alexScanTokens
  -> parseProgram
  -> analyzeProgram
      -> (erro) renderDiagnostics
      -> (ok) CheckedProgram
           -> evalProgram
                -> (erro runtime) mensagem
                -> (ok) imprime saida
```

## 9. Artefatos gerados vs autorais
Gerados:
- `Lexer.hs` (Alex)
- `Parser.hs`, `Parser.info` (Happy)

Autorais:
- AST
- semantica
- type checker
- interpretador
- CLI
- testes

## 10. Fases futuras
1. Codegen:
- transformar AST/IR para WAT/WASM.
2. Otimizacao:
- passes sobre AST/IR antes de codegen.

## 11. Politica de validacao dos exemplos (`test/examples`)
Para evitar interpretacao errada dos resultados, a avaliacao dos exemplos segue contrato por fase:

1. `--lexer`: todos devem passar.
2. `--parser`: todos devem passar.
3. `--pretty`: todos devem passar.
4. `--semantic`:
- `ex08_first_class_function_limit.sl` deve falhar (limitacao conhecida).
- demais devem passar.
5. `--interp`:
- `ex01`..`ex04`: devem passar.
- `ex05` e `ex06`: nao sao obrigatorios (sem `main`).
- `ex07`: deve falhar com erro de runtime (bounds).
- `ex08`: nao deve chegar aqui, pois falha antes na semantica.

## 12. Leitura correta de falhas

- Falha esperada (teste valido): confirma regra de seguranca/limitacao planejada.
- Falha nao esperada: indica regressao/bug.

Exemplo concreto:
- `Runtime error: Array index out of bounds` em `ex07` = comportamento correto.
- `Runtime error: Invalid lvalue assignment` em `ex02` (estado antigo) = bug de interpretador, corrigido com inicializacao default por tipo em declaracao tipada sem init.
