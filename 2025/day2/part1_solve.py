input = []
with open("input", "r") as f:
    line = f.readlines()[0].strip()
    input = line.split(",")


def is_invalid(num) -> bool:
    if len(num) % 2 == 0:
        invalid = True
        half = len(num) // 2
        for i in range(half):
            if num[i] != num[i + half]:
                invalid = False
                break
    else:
        invalid = False
    return invalid


def invalid_in_range(range_) -> int:
    left = int(range_[0])
    right = int(range_[1])
    sum = 0
    for num in range(left, right + 1):
        if is_invalid(str(num)):
            sum += int(num)
    return sum


ranges = list(map(lambda x: x.split("-"), input))

print(sum(map(invalid_in_range, ranges)))
