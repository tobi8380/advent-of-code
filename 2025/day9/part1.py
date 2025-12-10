input = []
with open("input", "r") as f:
    lines = f.readlines()
    input = list(
        map(lambda x: list(map(int, x)), map(lambda x: x.strip().split(","), lines))
    )


print(input)


max_x = max(input, key=lambda x: x[0])
min_x = min(input, key=lambda x: x[0])
max_y = max(input, key=lambda x: x[1])
min_y = min(input, key=lambda x: x[1])

max_xs = [x for x in input if x[0] == max_x[0]]
min_xs = [x for x in input if x[0] == min_x[0]]
max_ys = [x for x in input if x[1] == max_y[1]]
min_ys = [x for x in input if x[1] == min_y[1]]

extreme_points = max_xs + min_xs + max_ys + min_ys


def area(p, q):
    return (abs(p[0] - q[0]) + 1) * (abs(p[1] - q[1]) + 1)


print(extreme_points)
# for extreme_point in extreme_points:
print(max((map(lambda p: max((map(lambda x: area(x, p), input))), input))))
