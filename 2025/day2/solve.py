input = []
with open("input", "r") as f:
    line = f.readlines()[0].strip()
    input = line.split(",")


def compute_prefix_function(string) -> list[int]:
    m = len(string)
    prefix = [0 for _ in range(m)]
    prefix[0] = 0
    k = 0
    for q in range(1, m):
        while k > 0 and string[k] != string[q]:
            k = prefix[k - 1]
        if string[k] == string[q]:
            k += 1
        prefix[q] = k
    return prefix


def is_invalid(num) -> bool:
    num_str = str(num)
    m = len(num_str)
    for i in range(1, (m // 2) + 1):
        if m % i == 0:
            invalid = True
            for j in range(m):
                if num_str[j] != num_str[j % i]:
                    invalid = False
                    break
            if invalid:
                return True
    return False

    # num_str = str(num)
    # if len(num_str) == 1:
    #     return False

    # prefix_function = compute_prefix_function(num_str)
    # seen = []
    # longest_prefix = 0
    # j = 0
    # for i in range(1, len(num_str)):
    #     c = num_str[i]
    #     seen.append(c)
    #     # print(f"{c=}")
    #     # print(f"{j=}")
    #     # print(f"{num_str[j]=}")
    #     # print()
    #     if c == num_str[j]:
    #         j += 1
    #         if j > longest_prefix:
    #             longest_prefix = j
    #     else:
    #         j += 1
    #         while j > 0 and num_str[j] != c:
    #             j = prefix_function[j]

    # # print(f"{num=}")
    # # print(f"{longest_prefix=}")
    # if j == 0:
    #     # print(num)
    #     return False

    # repeated_prefix = len(num_str) - longest_prefix
    # # print(f"{repeated_prefix=}")
    # if j >= (len(num_str)) // 2 and len(num_str) % repeated_prefix == 0:
    #     for i, c in enumerate(num_str):
    #         if c != num_str[i % repeated_prefix]:
    #             return False

    #     return True
    # else:
    #     # print(num)
    #     return False


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
