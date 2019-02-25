#Rによる登山GPSログデータの分析  
##〜富士山と比良山地・武奈ヶ岳の登山行動の比較〜  

登山アプリ SkyWalking（http://deepkick.com/skywalking/ ）で取得したGPSログから作成したGPXファイルを、Rで分析し、グラフを作成するスクリプトを公開しました。  

例として、分析に使用したGPXファイルは、GPX_analysis/GPX_analysis/GPX/にあります。  
- 20180829_Fujisan_elevation_correction.gpx
- 20181111_Bunagatake_elevation_correction.gpx

出力したグラフは、GPX_analysis/GPX_analysis/Graphs/にあります。  

今回の分析を元にしてまとめたレポートは、GPX_analysis/repots/にあります。  

不十分なところもあると思いますが、RでGPXを分析するための参考になれば幸いです。

### 行動時間と非行動時間の割合

### 勾配とデータ分布の割合

### 勾配と平均垂直速度

### 勾配と平均水平速度

### 勾配と経過時間（180秒以上の停止状態を除外）

### 水平移動距離と標高

### 経過時間と標高

### 水平移動距離と相対高度

### 経過時間と相対高度

### 水平移動距離と累積上昇高度

### 水平移動距離と累積下降高度

### 勾配と水平速度の分布

#### 富士山の勾配と水平速度の分布

#### 比良の勾配と水平速度の分布

### 勾配と垂直速度の分布

#### 富士山の勾配と垂直速度の分布

#### 比良の勾配と垂直速度

## 結果

