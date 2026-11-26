from functools import reduce

def geomean(nums: list[float]) -> float:
    return reduce(lambda x, y: x * y, nums) ** (1 / len(nums))
