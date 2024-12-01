def part1():
    list1 = []
    list2 = []
    with open("inputs/day1/input.txt") as f:
        for _ in range(1000):
            line = f.readline()
            tokens = line.split("   ")
            list1.append(int(tokens[0]))
            list2.append(int(tokens[1]))

    list1 = sorted(list1)
    list2 = sorted(list2)
    diff = 0
    for i in range(1000):
        diff += abs(list1[i] - list2[i])
    return diff


def part2():
    list1 = []
    frequencies = {}
    with open("inputs/day1/input.txt") as f:
        for _ in range(1000):
            line = f.readline()
            tokens = line.split("   ")
            list1.append(int(tokens[0]))

            t2 = int(tokens[1])
            if t2 not in frequencies:
                frequencies[t2] = 0
            frequencies[t2] += 1
    sim_score = 0
    for i in list1:
        sim_score += i * frequencies.get(i, 0)

    return sim_score


if __name__ == "__main__":
    p1 = part1()
    print(f"Part 1: {p1}")

    p2 = part2()
    print(f"Part 2: {p2}")
