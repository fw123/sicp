#第一章
========================

>强力语言的三种机制：
>+ 1、基本表达形式：用于表示语言所关心的最简单的个体。
>+ 2、组合的方法：通过它们可以从较简单的东西出发构造出复合的元素。
>+ 3、抽象的方法：通过它们可以为复合对象命名，并将它们当做单元去操作。

>解释器工作过程:
>+ 1、对该组合式的各个子表达式求值。
>+ 2、将最左子表达式的值的那个过程用用于相应的实际参数，实际参数也即其他子表达式的值。

>过程定义：
>_(define (&lt;name&gt; &lt;formal parameters&gt;) (&lt;body&gt;))_
