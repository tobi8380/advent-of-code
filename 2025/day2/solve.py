input = []
with open("input", "r") as f:
    line = f.readlines()[0].strip()
    input = line.split(",")


def is_invalid(num) -> bool:
    num_str = str(num)
    if len(num_str) == 1:
        return False

    seen = []
    longest_prefix = 0
    j = 0
    for i in range(1, len(num_str)):
        c = num_str[i]
        seen.append(c)
        # print(f"{c=}")
        # print(f"{j=}")
        # print(f"{num_str[j]=}")
        # print()
        if c == num_str[j]:
            j += 1
            longest_prefix = j
        else:
            j = 0

    # print(longest_prefix)
    if j == 0:
        return False

    repeated_prefix = len(num_str) - longest_prefix

    if len(num_str) % repeated_prefix == 0:
        for i, c in enumerate(num_str):
            if c != num_str[i % j]:
                return False
        return True
    else:
        return False


def invalid_in_range(range_) -> int:
    left = int(range_[0])
    right = int(range_[1])
    sum = 0
    for num in range(left, right + 1):
        if is_invalid(num):
            # print(num)
            sum += num
    return sum


ranges = list(map(lambda x: x.split("-"), input))

print(sum(map(invalid_in_range, ranges)))
