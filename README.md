# Rによる登山GPSログデータの分析 〜富士山と比良山地・武奈ヶ岳の登山行動の比較〜  

登山アプリ SkyWalking（http://deepkick.com/skywalking/ ）で取得したGPSログから作成したGPXファイルを、Rで分析し、グラフを作成するスクリプトを公開しました。  
２つのGPXファイルを同じグラフに描画して比較する、といったことができます。

例として、分析に使用したGPXファイルは、
https://github.com/deepkick/GPX_analysis/tree/master/GPX_analysis/GPX
にあります。  

- 分析に使用したGPXファイル
  - [20180829_Fujisan_elevation_correction.gpx](https://github.com/deepkick/GPX_analysis/tree/master/GPX_analysis/GPX/20180829_Fujisan_elevation_correction.gpx)
  - [20181111_Bunagatake_elevation_correction.gpx](https://github.com/deepkick/GPX_analysis/tree/master/GPX_analysis/GPX/20181111_Bunagatake_elevation_correction.gpx) 

前者は2018年8月29日〜30日に渡って行った富士山登山のデータ（富士山・須走ルート）です。
後者は2018年11月11日に行った比良山系・武奈ヶ岳のデータ（比良山地・武奈ヶ岳への登山ルート）です。

メインのRスクリプトは、  
https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/gpx_analytics01.R  
にあります。  
以下のような２つのGPXデータの比較を行い、ggplot2 でグラフを作成しています。 

- 行動時間と非行動時間の割合
- 勾配とデータ分布の割合
- 勾配と平均垂直速度
- 勾配と平均水平速度
- 勾配と経過時間（180秒以上の停止状態を除外）
- 水平移動距離と標高
- 経過時間と標高
- 水平移動距離と相対高度
- 経過時間と相対高度
- 水平移動距離と累積上昇高度
- 水平移動距離と累積下降高度
- 勾配と水平速度の分布
  - 富士山の勾配と水平速度の分布
  - 比良の勾配と水平速度の分布
- 勾配と垂直速度の分布
  - 富士山の勾配と垂直速度の分布
  - 比良の勾配と垂直速度  

出力したグラフは、https://github.com/deepkick/GPX_analysis/tree/master/GPX_analysis/Graphs にあります。  

例えば、以下に示すようなグラフが作成できます。  
![経過時間と相対高度](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/01a_経過時間と相対高度.png)  
![水平移動距離と相対高度](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/01b_水平移動距離と相対高度.png)  
![経過時間と標高](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/02a_経過時間と標高.png)  
![水平移動距離と標高](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/02b_水平移動距離と標高.png)  
![勾配と平均垂直速度](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/04a_勾配と平均垂直速度.png)  
![勾配と平均水平速度](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/04b_勾配と平均水平速度.png)  
![富士山の勾配と垂直速度の散布図](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/04a_富士山の勾配と垂直速度の散布図.png)  
![比良の勾配と垂直速度の散布図](https://github.com/deepkick/GPX_analysis/blob/master/GPX_analysis/Graphs/04a_比良の勾配と垂直速度の散布図.png)  

今回のRによるGPXデータの分析について考察したレポートは、  
https://github.com/deepkick/GPX_analysis/blob/master/report/Seibutsuken_report_honda.pdf  
にあります。（約50Mbあります）  
このような分析をWeb上で手軽に出力できる方法を現在、考え中です。  

RでGPXを分析するための参考になれば幸いです。

## 参考文献・資料  
- フィールド情報学入門 : 自然観察, 社会参加, イノベーションのための情報学 / 京都大学フィールド情報学研究会編. -- 共立出版, 2009.  

- 地理空間データ分析 / 谷村晋著. -- 共立出版, 2010. -- (Rで学ぶデータサイエンス / 金明哲編集 ; 7).  

- 登山活動のGPSログ解析  
	http://yamakei.hatenadiary.jp/entry/2015/05/07/074113  

- 登山用ＧＰＳログ分析ツール　
	http://www.ne.jp/asahi/nature/kuro/HGPS/gps_log_analyze.htm  

- 急性高山病とは - 日本登山医学会　
	http://www.jsmmed.org/info/pg51.html  

- 富士登山 - ヤマレコの個人ページ　
	https://www.yamareco.com/modules/yamareco/detail-1572851.html  

- 武奈ヶ岳登山 - ヤマレコの個人ページ　
	https://www.yamareco.com/modules/yamareco/detail-1647671.html  

- オフラインマップ対応登山用GPSロギングiOSアプリ：SkyWalking　オフィシャルサイト  
	http://deepkick.com/skywalking/  
