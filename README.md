# ocaml_effect_analyzer

OCamlのソースコードからエフェクトの列を導出する解析器

## 使い方
cloneした本リポジトリをbuildする
```
dune build
```
**effect列の解析**
解析したいfileのpath(filepath)を指定して以下を実行
```
./_build/default/src/main.exe filepath
```

**グラフの描画**
以下を実行して初期設定を行う(2回目以降は不要)
```
cd graph
yarn install
```

以下を実行して，解析したfileを描画する
```
yarn build
yarn start
```



