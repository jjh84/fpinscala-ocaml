# Functional Programming in SCALA in OCaml

스칼라로 배우는 함수형 프로그래밍(폴 키우사노, 루나르 비아르드나손, 류광 번역)을 읽으면서
재밌어 보이는 코드를 OCaml로 작성함

## 실행 방법

`dune`을 이용하여 설치 가능 함

```bash
$ dune build
$ dune install
```

설치 후 `fpiso` 실행

```bash
$ fpiso

Functional Programming in Scala in OCaml

Usage:
    fpiso COMMAND

Available Commands:
    candyMachine
    wc
```

## 요구사항

Monad, Applicative를 위한 (Let_syntax)[https://caml.inria.fr/pub/docs/manual-ocaml/bindingops.html]를 이용하기 때문에 OCaml 컴파일러 4.08.0 이상에서 컴파일 가능하다.