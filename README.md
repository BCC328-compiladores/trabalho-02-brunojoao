[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/X15MPpfH)
# Trabalho prático de BCC328 - Construção de Compiladores I

## Suporte de Sintaxe .sl no Visual Studo Code

Para visualizar os arquivos `.sl` com destaque de sintaxe no VS Code, siga os passos abaixo:

### Instalação

1. Localize o arquivo `sl-recognizer-0.0.1.vsix` na pasta `/vs-code-extensions` dentro do projeto do trabalho.  
2. No terminal rode:

```bash
code --install-extension vs-code-extensions/sl-recognizer-0.0.1.vsix
```

## Iniciando o Container

Para iniciar o ambiente Docker:

```
docker-compose up -d
```
```
docker-compose exec sl bash
```

Isso abrirá um terminal dentro do container, já no diretório /workspace.

## Gerar o Relatório

```
pdflatex -output-directory=relatorio relatorio/relatorio.tex
```

## Compilar o projeto

```
cabal update
```

```
cabal install --installdir=. --overwrite-policy=always -v0
```

## Rodar Lexer

```
./compiler --lexer test/examples/ex01_factorial.sl
```

## Rodar Parser

```
./compiler --parser test/examples/ex01_factorial.sl
```

## Rodar Pretty

```
./compiler --pretty test/examples/ex01_factorial.sl
```

## Rodar Analisador Semantico (Stage 2)

```
./compiler --semantic test/examples/ex01_factorial.sl
```

## Rodar Interpretador (Stage 2)

```
./compiler --interp test/examples/ex01_factorial.sl
./compiler --interp test/examples/ex04_bmi_booleans.sl
```

## Limitacoes conhecidas (Stage 2)

- Chamada de funcao por valor (first-class function em call site) ainda nao esta completa.
- Bounds check de array gera erro de runtime por projeto (comportamento esperado e testado).

## Demos de falha para apresentacao academica

Falha semantica (funcao como valor no call site, limitacao atual):

```
./compiler --semantic test/examples/ex08_first_class_function_limit.sl
```

Falha de runtime (bounds check em array):

```
./compiler --interp test/examples/ex07_array_oob_runtime.sl
```

Para casos automatizados de falha esperada, veja:

```
test/TestLimitations.hs
```

## Demo em 2 minutos (roteiro pronto)

1. Caso de sucesso (recursao):
```bash
./compiler --interp test/examples/ex01_factorial.sl
```
2. Caso de sucesso (tipos + if/else):
```bash
./compiler --interp test/examples/ex04_bmi_booleans.sl
```
3. Falha esperada de semantica:
```bash
./compiler --semantic test/examples/ex08_first_class_function_limit.sl
```
4. Falha esperada de runtime:
```bash
./compiler --interp test/examples/ex07_array_oob_runtime.sl
```

## Rodar testes

Compila e executa todos os testes configurados:

```
cabal test
```

## Limpar os codigos compilados.

```
cabal clean
```
