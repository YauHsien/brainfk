# 實作 brainfuck 程式語言

[語言參考](https://esolangs.org/wiki/Brainfuck)

#### 實作進度：架構完成但結果錯誤

例如：原本希望取得 `Prog` 為印出 "Hello, World" ，但是，

```Prolog
?- machine(M), case(Prog), eval(Prog,M,M2).
elo, Worme͡
M = (pointer(0), cells([])),
Prog = [>, +8, loop([<, +9, >, -]), >, '.', <, +4, loop([...|...]), <|...],
M2 = (pointer(-1), cells([(-1, 865), (0, 0), (1, 87), (2, 0)])).
```
