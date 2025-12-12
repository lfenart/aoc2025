import pulp

def parse(line: str) -> tuple[list[list[int]], list[int]]:
    tokens = line.strip().split(" ")
    buttons = tokens[1:-1]
    buttons = [[int(x) for x in button[1:-1].split(",")] for button in buttons]
    joltages = [int(x) for x in tokens[-1][1:-1].split(",")]
    return (buttons, joltages)

def solve(a: list[list[int]], b: list[int]) -> int:
    n = len(a[0])
    prob = pulp.LpProblem(sense=pulp.LpMinimize)

    x = [pulp.LpVariable(f"x{i}", lowBound=0, cat=pulp.LpInteger) for i in range(n)]

    prob += pulp.lpSum(x)

    for i in range(len(a)):
        prob += pulp.lpSum(a[i][j]*x[j] for j in range(n)) == b[i]

    prob.solve()

    return sum(xi.varValue for xi in x)

data = []
with open("input/puzzle10.txt", "r") as f:
    for line in f.readlines():
        data.append(line)

parsed = [parse(line) for line in data]

acc = 0
for (buttons, joltages) in parsed:
    matrix = []
    for i in range(len(joltages)):
        matrix.append([0] * len(buttons))
    for i, button in enumerate(buttons):
        for x in button:
            matrix[x][i] = 1
    acc += solve(matrix, joltages)
print(acc)
