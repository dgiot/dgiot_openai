# dgiot_openai

dgiot_openai是一个erlang语言实现的开源插件，主要功能是实现chatGPT的api封装和桥接。

## 功能特性

* 封装chatGPT的api，提供简单易用的接口，方便调用
* 支持多种语言，支持中文、英文等
* 支持多种模式，支持问答模式、对话模式等

## 安装

### 依赖

* Erlang/OTP 20.0+

### 下载

```
git clone https://github.com/dgiot/dgiot_openai.git
```

### 编译

```
cd dgiot_openai
make
```

## 使用

### 初始化

```erlang
dgiot_openai:start().
```

### 调用

```erlang
dgiot_openai:query(Text, Mode).
```

其中，Text为要查询的文本，Mode为查询模式，可以是`qa`（问答模式）或`dialog`（对话模式）。

## 示例

```erlang
1> dgiot_openai:start().
ok
2> dgiot_openai:query("你叫什么名字？", qa).
{ok,{[{text,"我叫dgiot_openai。"}],
[{type,"qa"},{confidence,1.0}]}}
3> dgiot_openai:query("你好！", dialog).
{ok,{[{text,"你好！很高兴认识你。"}],
[{type,"dialog"},{confidence,1.0}]}}
```
