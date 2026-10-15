import os
import re

def update_sv_modules_and_filelist(directory):
    module_name_map = {}
    file_path_map = {}

    # Step 1: 找出模块定义和路径映射
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith('.sv'):
                old_path = os.path.join(root, file)
                with open(old_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                    matches = re.findall(r'^\s*module\s+(\w+)\b', content, flags=re.MULTILINE)
                    if matches:
                        for module_name in matches:
                            if module_name.startswith('BY_'):
                                continue
                            module_name_map[module_name] = f'BY_{module_name}'
                        new_file_name = file if file.startswith('BY_') else f'BY_{file}'
                        new_path = os.path.join(root, new_file_name)
                        file_path_map[old_path] = new_path

    # Step 2: 修改模块名和实例引用
    for old_path, new_path in file_path_map.items():
        with open(old_path, 'r', encoding='utf-8') as f:
            content = f.read()

        for old_name, new_name in sorted(module_name_map.items(), key=lambda item: len(item[0]), reverse=True):
            content = re.sub(rf'\bmodule\s+{old_name}\b', f'module {new_name}', content)
            content = re.sub(rf'\b{old_name}\b(?=\s*(?:#\s*\(|\w+\s*\())', new_name, content)

        with open(old_path, 'w', encoding='utf-8') as f:
            f.write(content)

    # Step 3: 文件重命名
    for old_path, new_path in file_path_map.items():
        os.rename(old_path, new_path)
        print(f'Renamed: {os.path.basename(old_path)} → {os.path.basename(new_path)}')

    # Step 4: 简单处理 filelist.f 中每一行，加 hello_ 前缀
    filelist_path = os.path.join(directory, 'filelist.f')
    if os.path.exists(filelist_path):
        with open(filelist_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()

        updated_lines = []
        for line in lines:
            stripped = line.strip()
            if stripped and stripped.endswith('.sv'):
                path, name = os.path.split(stripped)
                updated_lines.append(os.path.join(path, name if name.startswith('BY_') else f'BY_{name}'))
            else:
                updated_lines.append(line.strip())

        with open(filelist_path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(updated_lines) + '\n')
        print('Updated filelist.f')

# 使用方法
if __name__ == "__main__":
    target_dir = "./build"  # 修改为实际路径
    update_sv_modules_and_filelist(target_dir)
