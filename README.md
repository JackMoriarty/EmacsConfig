# My Simple Emacs Config
这是一个简易的emacs配置文件

## 前置要求
1. emacs >= 29

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

3. 下载并安装[Maple Mono NF CN](https://github.com/subframe7536/maple-font/releases)字体并安装，如果在终端环境下使用emacs，可将终端字体设置为`Maple Mono NF CN`

4. 启动emacs，并等待插件安装完成

5. 安装icon字体 **(终端环境无需配置)**

```bash
M-x nerd-icons-install-fonts
```

6. **LLM 插件支持**
如果只使用本地通过ollama部署的LLM模型，可以根据自身实际需要修改配置文件中相关LLM插件的模型。
如果使用云厂商提供的LLM API，需要提供API key，API key 需要保存在home目录下的`.authinfo`文件中

```bash
touch $HOME/.authinfo
```

该文件的格式为
```
machine <host> login <user> password <api_key>
```
比如
```
machine api.groq.com login apikey password <api_key>
```

当前配置文件中的LLM插件有

| 插件名称               | 功能                           |
|------------------------|--------------------------------|
| go-translate           | 翻译                           |
| gptel                  | gpt chat                       |
| insert-translated-name | 在插入点将输入的中文转换为英文 |
| ollama-buddy           | ollama本地模型管理与chat       |
| minuet                 | 代码补全                       |

7. **（可选）远程开发**
通过ssh和rsync实现本地与服务器的文件同步。本机与服务器端都需要安装ssh和rsync。
对于需要同步的项目，在项目内执行`M-x ppcompile-config-project`命令，根据指引填写配置。

| 命令                 | 作用                   |
|----------------------|------------------------|
| `M-x ppcompile-ping` | 上传项目到服务器       |
| `M-x ppcompile-pong` | 在服务器上编译项目     |
| `M-x ppcompile`      | 上传项目到服务器并编译 |

更多配置及使用参见[ppcompile](https://github.com/whatacold/ppcompile)

ssh还需做如下配置
```
# ssh连接复用
Host *
    ControlMaster auto
    ControlPath ~/.ssh/control-%r@%h:%p
    ControlPersist 600
```
