import argparse
from python.day1 import part1 as day1_part1, part2 as day1_part2


def main():
    parser = argparse.ArgumentParser(description="Advent of Code 2024 Solutions")
    parser.add_argument("--day", type=int, help="Which day to run")

    args = parser.parse_args()

    if args.day == 1:
        print("Day 1:")
        p1 = day1_part1()
        p2 = day1_part2()
        print(f"Part 1: {p1}")
        print(f"Part 2: {p2}")
    else:
        print(f"Day {args.day} not implemented yet")


if __name__ == "__main__":
    main()
