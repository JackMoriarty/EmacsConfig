# My Simple Emacs Config
这是一个简易的emacs配置文件

## 前置要求
1. emacs >= 28
2. python >= 3.7

## 安装
1. 下载本项目
```bash
cd $HOME
git clone https://github.com/JackMoriarty/EmacsConfig .emacs.d
```
2. **(可选)** 对于终端环境，需要在Shell初始化文件中设置如下环境变量，以确保颜色显示正确，对于bash来说为`.bashrc`。
```bash
export COLORTERM=truecolor
```

3. 下载并安装[FiraCode](https://github.com/tonsky/FiraCode)字体并安装，如果在终端环境下使用emacs，可将终端字体设置为FireCode。

4. 安装插件依赖的软件包
```bash
# lsp-bridge依赖包
pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz
```

5. 启动emacs，并等待插件安装完成

6. 安装icon字体 **(终端环境无需配置)**

```bash
M-x all-the-icons-install-fonts
```

7. 配置eaf，安装需要的app, 本配置文件中默认需要以下app，可以根据需要修改。**(终端环境无需配置)**

* eaf-browser
* eaf-jupyter
* eaf-markdown-previewer

  执行以下命令，并启用上述软件包。
```bash
M-x eaf-install-and-update
```
> 关于EAF更多的软件包以及使用用法，可以参见[EAF](https://github.com/emacs-eaf/emacs-application-framework)。

8. 安装代码分析和补全后端
```bash
# python语言
pip3 install pyright ruff-lsp
# C/C++, 需要clangd后端，ArchLinux用户使用以下命令安装
yay -S clang
```
如果需要其他语言的后端，可在[LSP-Bridge](https://github.com/manateelazycat/lsp-bridge)找到相关配置方法。

9. **(可选)** 安装treesit所有语法包
```bash
M-x treesit-auto-install-all
```
