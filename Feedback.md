# Comments

- first of all, congratulations for your excellent work!
- the erlang sources you provide are compatible only with erlang/otp >= 21, after a couple of fixes (see my commit) your erlang-code runs smoothly but there is still an issue (see log below)

```bash
$ python3 script.py
0 51
Traceback (most recent call last):
  File "script.py", line 66, in <module>
    listacc[i][elem-1]+=1
IndexError: list index out of range

```

- let me also point out the importance of your the nomenclature, for instance the use of the module *network*, this confirms your understanding of concepts seen in class and improves the understanding of your code
- your sources are easy to read and provide comments that allows one to follow the logic in your algorithms
- your report is quite brief, even though the sources have enough comments to follow the choices in your implementation, it is clear that there is a lack of argumentation that explain the behavior of curves, you describe how the curves behave with no further details. Here one example of what was expected: *the observed variance of in-degree when nodes recover reflects that the in-degree is not equally balanced among all nodes in the network (as shown before nodes crash); or even simpler, there are partitions where one observe that certain clusters contain more nodes than others*. On the other hand, one can see the convergence of the PS service when the variance of the in-degree narrows during the recovery phase 

# Grade
| Bootstrap network (20%) | PS service implementation (50%) | Experimental scenario (30%) | Grade in % | Points (up to 5) |
|---|---|---|---|---|
|20 |	50|	15|	85|	4.25|
