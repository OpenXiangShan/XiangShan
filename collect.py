import os
import glob
import re

# 从单个文件中提取匹配模式后的数字并求和
def sum_numbers_in_file(file_path, pattern):
    total = 0
    # 编译正则表达式，捕获 pattern 后的数字
    regex = re.compile(pattern + r",\s*(\d+)")
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            for line in f:
                match = regex.search(line)
                if match:
                    number_str = match.group(1)  # 获取捕获的数字字符串
                    try:
                        number = int(number_str)  # 转换为整数
                        total += number
                    except ValueError:
                        print(f"警告：在文件 {file_path} 中无法将 '{number_str}' 转换为整数，已跳过")
    except Exception as e:
        print(f"读取文件 {file_path} 时出错: {e}")
    return total

# 处理文件夹，统计其中所有 txt 文件中匹配模式的数字和
def process_folder(folder_path, pattern):
    folder_total = 0
    for file_name in os.listdir(folder_path):
        if file_name.endswith(".txt"):
            file_path = os.path.join(folder_path, file_name)
            folder_total += sum_numbers_in_file(file_path, pattern)
    return folder_total

# 主函数，处理多个前缀的文件夹并统计匹配模式的数字和
def main(base_path, prefixes, pattern):
    grand_total = 0
    for prefix in prefixes:
        # 构建通配符路径，例如 "./SPEC06_EmuTasks_0327_0925_lvp63/gcc*"
        path_pattern = os.path.join(base_path, prefix + "*")
        matched_folders = glob.glob(path_pattern)  # 获取匹配的文件夹路径

        if not matched_folders:
            print(f"未找到以 '{prefix}' 开头的文件夹")
            continue

        prefix_total = 0
        for folder in matched_folders:
            if os.path.isdir(folder):
                folder_total = process_folder(folder, pattern)
                print(f"文件夹 {folder} 中匹配 '{pattern}' 的数字和: {folder_total}")
                prefix_total += folder_total
            else:
                print(f"警告: {folder} 不是文件夹，已跳过")

        print(f"以 '{prefix}' 开头的文件夹中匹配 '{pattern}' 的数字总和: {prefix_total}")
        grand_total += prefix_total

    print(f"所有指定前缀文件夹中匹配 '{pattern}' 的数字总和: {grand_total}")

# 运行示例
if __name__ == "__main__":
    base_path = "./SPEC06_EmuTasks_0327_1837_lvp69"  # 基础路径
    prefixes = ["perlbench", "bzip2", "gcc", "mcf", "gobmk", "hmmer", "sjeng", "libquantum", "h264ref", "omnetpp", "astar", "xalancbmk"]  # 要查询的文件夹前缀
    pattern = r"misPred_total"  # 要查找的字符串模式
    main(base_path, prefixes, pattern)