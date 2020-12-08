from os import system as sys
import re
import statistics
import subprocess
# def : function who lauch erlang function for the test
def launch():
    sys("erlc network.erl")
    sys("erlc node_functions.erl")
    sys("erlc node.erl")
    sys("erlc test.erl")

    sys("erl -noshell -eval 'test:swap_tail()' -run init stop > log.txt")
# def : function who transform string to integer
# parameters : string String
def to_ID(string):

    return int(string)
# Starting
launch()
# log file for computing the indegree and the standard deviation
file = open("log.txt", 'r')
fileR = str(file.read())
source = fileR

# create only on node who are instanciate
counter = 0
listacc = [[] for i in range(180)]
for i in range(180):
    if counter < 30:
        listacc[i] = [0 for j in range(51)]
    elif counter < 60:
        listacc[i] = [0 for j in range(76)]
    elif counter < 90:
        listacc[i] = [0 for j in range(102)]
    else:
        listacc[i] = [0 for j in range(128)]
    counter +=1
#list of concatenate id per cycle
listC = []

p = re.compile('log::.*')

cycle0 = 1
viewconcat = []
for log in re.findall(p, source):
    #log = 1 line
    logs = log.split(" ")
    cycles = int(logs[1])

    # if the list is empty
    if len(logs[3]) > 2:
        view = logs[3].strip('][').split(',')
        idview = map(to_ID, view)
    # if we are on the same cycle
    if cycles == cycle0:
        viewconcat += idview
        pass
    else:
        cycle0 += 1
        listC.append(viewconcat)
        viewconcat = list(idview)
# acc for increasing the indegree of each node
for i in range(0, len(listC)):
    for elem in listC[i]:
        print(i,elem-1)
        listacc[i][elem-1]+=1

# close the log, we have all the date needed
file.close()
count = 0
for acc in listacc:
    # 20 - 40 - 60 - 80 - 100 - 120 - 160
    if (count%20 == 0):
        mathFile = open("swapper_deployment_tail.data", 'a')
        mean = statistics.mean(acc)
        dev = statistics.stdev(acc)
        mathFile.write(str(count) + " " + str(mean) + " " + str(dev) + "\n")
        mathFile.close()
    # 151 when we are restoring
    elif (count%151 == 0):
        mathFile = open("swapper_deployment_tail.data", 'a')
        mean = statistics.mean(acc)
        dev = statistics.stdev(acc)
        mathFile.write(str(151) + " " + str(mean) + " " + str(dev) + "\n")
        mathFile.close()
    # the real 180 ending
    elif (count%178 == 0):
        mathFile = open("swapper_deployment_tail.data", 'a')
        mean = statistics.mean(acc)
        dev = statistics.stdev(acc)
        mathFile.write(str(180) + " " + str(mean) + " " + str(dev) + "\n")
        mathFile.close()
    count = count + 1
