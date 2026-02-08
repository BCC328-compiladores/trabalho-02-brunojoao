# Roteiro de Defesa (Apresentacao Oral)

## 1) Abertura (30-45s)

"O projeto implementa um compilador/interprete para a linguagem SL em Haskell, com pipeline completa de frontend e execucao: lexer, parser, AST, analise semantica com checagem de tipos/escopo/funcoes e interpretador. O backend de codegen e as otimizacoes ficam como evolucao futura."

## 2) Pipeline (1 min)

"O fluxo principal e: codigo fonte -> tokens -> AST -> analise semantica -> interpretacao."

```text
source
  -> Lexer (Alex)
  -> Parser (Happy)
  -> AST
  -> Semantic/Type Checker
  -> Interpreter
```

"Na CLI, isso e exposto pelas flags `--lexer`, `--parser`, `--pretty`, `--semantic` e `--interp`."

## 3) Fronteira Gerado vs Manual (1 min)

"No projeto, apenas tres arquivos sao gerados automaticamente: `src/Lexer.hs`, `src/Parser.hs` e `src/Parser.info`. A fonte real e mantida em `src/Lexer.x` e `src/Parser.y`, e toda a logica de semantica e interpretador e manual."

## 4) AST e Modelo de Linguagem (1-2 min)

"A AST em `src/AST.hs` e o contrato entre todas as fases. Ela modela tipos primitivos, arrays, structs, funcoes, generics (`forall` com `funcTParams`), controle de fluxo (`if/while/for`), retorno e expressoes como chamada de funcao, acesso a array, `.size` e `post-inc`."

"Decisao chave: manter a AST suficientemente expressiva para parser e semantica, mas sem acoplamento com detalhes de runtime."

## 5) Analise Semantica e Tipos (2-3 min)

"A analise semantica e orquestrada por `src/Semantic/Analyzer.hs`, apoiada por ambientes especializados:"

- `Environment/TypeEnv.hs`: tipos de variaveis por escopo.
- `Environment/FuncEnv.hs`: assinaturas de funcoes.
- `Environment/StructEnv.hs`: definicoes de structs.
- `Environment/SymbolTable.hs`: estrutura de escopos.

"Checkers dedicados reforcam separacao de responsabilidades:"

- `ScopeChecker`: identificadores e escopo.
- `FunctionChecker`: declaracoes/chamadas/retorno.
- `StructChecker`: campos e acessos.

"A camada `TypeChecker` (`Types`, `Infer`, `Unify`) realiza inferencia e compatibilidade de tipos. Os erros saem padronizados por `Errors/Diagnostic.hs` e formatados em `Errors/Pretty.hs`."

## 6) Interpretador (2 min)

"O runtime esta em `src/Interpreter/Eval.hs`, com dominio de valores em `Value.hs` e builtins em `Builtins.hs`."

"A execucao usa estado de runtime com escopos e tratamento de erro monadico. Isso viabiliza:"

- chamada de funcao e recursao;
- `if/while/for`;
- arrays e structs;
- efeitos observaveis como `print`.

"No modo `--interp`, a semantica roda antes da execucao. Se houver erro semantico, a interpretacao nao inicia."

## 7) Qualidade e Testes (1 min)

"A suite tem testes por fase:"

- `TestLexer.hs` e `TestParser.hs` para frontend;
- `TestSemantic.hs` para regras estaticas;
- `TestInterpreter.hs` para comportamento dinamico.

"Isso protege regressao de Stage 1 e valida extensoes de Stage 2."

## 7.1) Falha esperada vs bug real (30s)

"Neste projeto, alguns casos de erro sao intencionais e fazem parte da especificacao de limites atuais."

- **Falha esperada:** quando um teste confirma que o compilador rejeita algo nao suportado.
- **Bug real:** quando um caso suportado falha sem previsao.

"Exemplo de falha esperada: `ex08_first_class_function_limit.sl`, que documenta limitacao atual de funcao como valor no call site. Exemplo de protecao de runtime: `ex07_array_oob_runtime.sl`, que valida bounds check."

## 8) Limites Atuais e Proximos Passos (1 min)

"Hoje o projeto cobre frontend + semantica + interpretacao. Ainda nao gera codigo alvo e nao possui passagens de otimizacao."

"A evolucao planejada e Stage 3 com backend WASM (WAT emitter, modelo de pilha e runtime) e depois camada de otimizacoes (const folding, DCE, simplificacoes CFG)."

## 9) Encerramento (20-30s)

"Em termos de arquitetura, o projeto esta modular, com separacao clara entre parse, semantica e runtime. Em termos academicos, ele ja permite discutir teoria de linguagens, inferencia de tipos, escopo, interpretacao e engenharia de compiladores com base em codigo real e testado."

## 10) Demo curta (2 min)

1. `./compiler --interp test/examples/ex01_factorial.sl`
2. `./compiler --interp test/examples/ex04_bmi_booleans.sl`
3. `./compiler --semantic test/examples/ex08_first_class_function_limit.sl`
4. `./compiler --interp test/examples/ex07_array_oob_runtime.sl`

---

## Perguntas Comuns (respostas curtas)

1. **Por que usar Alex/Happy?**
   "Para ter lexer/parser declarativos e reproduziveis, focando esforco na semantica e runtime."
2. **Onde entra a checagem de tipos?**
   "Na fase semantica, em `Semantic.Analyzer` com apoio de `TypeChecker/*`."
3. **O interpretador executa AST diretamente?**
   "Sim. Depois da semantica aprovar, `runProgram` avalia a AST."
4. **O que falta para virar compilador completo?**
   "Backend de codegen (ex.: WASM) e passagens de otimizacao."
