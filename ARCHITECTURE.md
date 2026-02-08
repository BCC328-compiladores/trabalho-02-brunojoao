# Arquitetura do Compilador SL

## 1. Visao geral
Este repositorio implementa um compilador/interpretador para a linguagem SL com as fases:

`codigo-fonte -> lexer () -> parser -> AST -> analise semantica/tipos -> interpretador`

O projeto esta organizado no diretorio `workspace/`:

- `workspace/src`: implementacao da linguagem.
- `workspace/app`: CLI.
- `workspace/test`: testes de lexer, parser, semantica e interpretador.
- `workspace/compiler.cabal`: declaracao de pacotes, modulos e executaveis.

## 2. Diagrama de arquitetura
```text
                         +---------------------+
 Fonte .sl  -----------> | Lexer (Alex)        |
                         | src/Lexer.x/.hs     |
                         +----------+----------+
                                    |
                                    v
                         +----------+----------+
                         | Parser (Happy)      |
                         | src/Parser.y/.hs    |
                         +----------+----------+
                                    |
                                    v
                         +----------+----------+
                         | AST                  |
                         | src/AST.hs           |
                         +----------+----------+
                                    |
                     +--------------+------------------+
                     |                                 |
                     v                                 v
          +----------+----------+           +----------+----------+
          | AST Pretty / Code   |           | Analise Semantica   |
          | src/ASTPretty.hs    |           | src/Semantic/*      |
          | src/CodePretty.hs   |           | src/TypeChecker/*   |
          +---------------------+           | src/Environment/*   |
                                            | src/Errors/*        |
                                            +----------+----------+
                                                       |
                                                       v
                                            +----------+----------+
                                            | Interpretador       |
                                            | src/Interpreter/*   |
                                            +---------------------+
```

## 3. Separacao de responsabilidades
- **Frontend sintatico**:
  - Reconhece texto e estrutura.
  - Nao decide regras semanticas profundas.
- **Semantica e tipos**:
  - Verifica escopo, tipos, chamadas, structs, arrays, retornos.
  - Construi ambientes para funcao, struct e variaveis.
- **Execucao**:
  - Avalia AST validada.
  - Mantem estado de runtime (escopos, saida, funcoes, structs).

## 4. Decisoes arquiteturais principais
1. **AST unica compartilhada** (`src/AST.hs`):
- simplifica parser, pretty e interpretador.
- tradeoff: AST ainda nao carrega spans por no.

2. **Semantica centralizada em `Semantic.Analyzer`**:
- modulo unico com fluxo completo de validacao.
- modulos `ScopeChecker`, `FunctionChecker`, `StructChecker` hoje sao wrappers.

3. **Tipos semanticos separados de tipos sintaticos**:
- `AST.Type` representa o que foi parseado.
- `TypeChecker.Types.SType` representa tipo em verificacao semantica.

4. **Interpretador com monad de estado + erro**:
- `ExceptT String (State Runtime)` em `Interpreter.Eval`.
- permite erro de runtime com estado explicito.

5. **Geracao de parser/lexer mantida no repositorio**:
- fontes: `Lexer.x` e `Parser.y`.
- gerados: `Lexer.hs`, `Parser.hs`, `Parser.info`.

## 5. Estado funcional atual
Implementado:
- Lexer, Parser, AST.
- Pretty AST e pretty codigo.
- Analise semantica e type checker.
- Interpretador.
- CLI: `--lexer`, `--parser`, `--pretty`, `--semantic`, `--interp`.
- Testes automatizados para todas as fases atuais.

Nao implementado (futuro):
- Code generation (WASM/WAT).
- Otimizacoes (passes de IR/AST).

## 5.1 Tabela de status por feature
| Feature | Status | Referencia |
|---|---|---|
| Lexer | Implementado | `workspace/src/Lexer.x` |
| Parser | Implementado | `workspace/src/Parser.y` |
| AST | Implementado | `workspace/src/AST.hs` |
| Semantica e tipos | Implementado | `workspace/src/Semantic/Analyzer.hs` |
| Interpretador | Implementado | `workspace/src/Interpreter/Eval.hs` |
| Testes de sucesso | Implementado | `workspace/test/TestLexer.hs`, `workspace/test/TestParser.hs`, `workspace/test/TestSemantic.hs`, `workspace/test/TestInterpreter.hs` |
| Testes de limitacao/falha esperada | Implementado | `workspace/test/TestLimitations.hs` |
| Funcoes de primeira classe em call site | Parcial (limitacao conhecida) | `workspace/test/examples/ex08_first_class_function_limit.sl` |
| Codegen (WASM/WAT) | Futuro | `COMPILER_PIPELINE.md` |
| Otimizacoes | Futuro | `COMPILER_PIPELINE.md` |

## 6. Integracao no executavel
`workspace/app/Main.hs` conecta as fases:

- `--lexer`: `alexScanTokens`.
- `--parser`: `parseProgram` + `ASTPretty.programTree`.
- `--pretty`: `parseProgram` + `CodePretty.prettyProgram`.
- `--semantic`: `analyzeProgram`.
- `--interp`: `analyzeProgram` + `evalProgram`.

A semantica sempre roda antes da interpretacao.

## 7. Erros e diagnosticos
Camada de erros:
- `src/Errors/Diagnostic.hs`
- `src/Errors/Pretty.hs`

Estrutura:
- codigo (`diagCode`)
- severidade
- posicao opcional
- mensagem
- hint opcional

Fluxo:
- semantica retorna `Either [Diagnostic] ...`
- CLI renderiza com `renderDiagnostics`.

## 8. Observacoes para manutencao
1. Sempre editar `Lexer.x` e `Parser.y`, depois regenerar.
2. Evitar editar manualmente `Lexer.hs` e `Parser.hs`.
3. Ao adicionar feature de linguagem:
- atualizar AST
- atualizar parser/lexer
- atualizar semantica
- atualizar interpretador
- adicionar testes em `workspace/test`.

## 9. Limites atuais conhecidos
1. Diagnosticos sem span por no da AST.
2. Sem backend de codigo.
3. Sem pipeline de otimizacao dedicada.
4. `Semantic.ScopeChecker`, `FunctionChecker`, `StructChecker` ainda sao fachadas para `analyzeProgram`.
