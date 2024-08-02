# My Simple Emacs Config
这是一个简易的emacs配置文件

## 前置要求
1. emacs >= 28
2. python >= 3.7

## 安装
1. 在$HOME目录下下载本项目
```bash
cd $HOME
git clone https://github.com/JackMoriarty/EmacsConfig .emacs.d
```
2. **(可选)** 对于终端环境，需要在Shell初始化文件中设置如下环境变量，以确保颜色显示正确，对于bash来说为`.bashrc`。
```bash
export COLORTERM=truecolor
```

3. 下载并安装[FiraCode Nerd Font Mono](https://github.com/ryanoasis/nerd-fonts/releases)和[Source Han Sans CN (Region-specific subset OTF)](https://github.com/adobe-fonts/source-han-sans/tree/release)字体并安装，如果在终端环境下使用emacs，可将终端字体设置为FireCode Nerd Font Mono

4. 启动emacs，并等待插件安装完成

5. 安装icon字体 **(终端环境无需配置)**

```bash
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
```

6. 配置eaf，安装需要的app, 本配置文件中默认需要以下app，可以根据需要修改。**(终端环境无需配置)**

* eaf-browser
* eaf-jupyter
* eaf-markdown-previewer

  执行以下命令，并启用上述软件包。
```bash
M-x eaf-install-and-update
```
> 关于EAF更多的软件包以及使用用法，可以参见[EAF](https://github.com/emacs-eaf/emacs-application-framework)。

7. **(可选)** 安装treesit所有语法包
```bash
M-x treesit-auto-install-all
```
