line.r -f line.data -x Pos -y value -l Variable -c Variable -m TRUE
line.r -f line.data -x Pos -y value -m TRUE
line.r -f line.data -x Pos -y value -l Variable -m TRUE -s auto -V "cyan,purple"
# 下行代码在R脚本中运行，图中文字带反斜线
line.r -f line.data -x Pos -y value -l Variable -m TRUE -s auto -V "cyan,purple" -b "1000,2000" -P "100
0,4000" -B '\-1 kb,1 kb' -U "\-5 kb,5 kb"

splot_line.sh -f line.data -x Pos -y value -l Variable -c Variable -m TRUE
splot_line.sh -f line.data -x Pos -y value -m TRUE
splot_line.sh -f line.data -x Pos -y value -l Variable -m TRUE -s auto -V "cyan,purple"
splot_line.sh -f line.data -x Pos -y value -l Variable -m TRUE -s auto -V "cyan,purple" -P "-5000,0,5000" -
b "-1000,1000" -B "-1 kb,1 kb" -U "-5 kb,TSS,5 kb"


flowerplot.r -f flower.txt -c green
flowerplot.r -f flower.txt -A 1 -B 1.5 -r 1 -n FALSE -c Set2


splot_flowerplot.sh -f flower.txt -c green
splot_flowerplot.sh -f flower.txt -A 1 -B 1.5 -r 1 -n FALSE -c Set2



sp_boxplot.r -f box.txt  -m T -x Gene -y Expr -l Group
sp_boxplot.r -f box.txt  -m T -x Gene -y Expr -l Group  -z T -V T
sp_boxplot.r -f box.txt  -m T -x Gene -y Expr -l Group -M Set3 -v T -F T
sp_boxplot.r -f box.txt  -m T -x Gene -y Expr -l Group -M 'green,yellow,red'  -z T -k T  -v T
sp_boxplot.r -f box.txt  -m T -x Gene -y Expr -l Group -M 'green,yellow,red'  -z T -k T  -v T
sp_boxplot.r -f box.txt  -m T -x Gene -y Expr -l Group -M Set3 -v T -e Group -N 3 -W 1


splot_boxplot.sh -f box.txt  -m T -x Gene -y Expr -l Group
splot_boxplot.sh -f box.txt  -m T -x Gene -y Expr -l Group  -z T -V T
splot_boxplot.sh -f box.txt  -m T -x Gene -y Expr -l Group -M Set3 -v T -F T
splot_boxplot.sh -f box.txt  -m T -x Gene -y Expr -l Group -M 'green,yellow,red'  -z T -k T  -v T
splot_boxplot.sh -f box.txt  -m T -x Gene -y Expr -l Group -M 'green,yellow,red'  -z T -k T  -v T
splot_boxplot.sh -f box.txt  -m T -x Gene -y Expr -l Group -M Set3 -v T -e Group -N 3 -W 1




sp_pheatmap.r -f exprTable.txt -A 90 -c TRUE -d TRUE
sp_pheatmap.r -f exprTable.txt -z Set2
sp_pheatmap.r -f exprTable.txt -z "green,red" -s row
sp_pheatmap.r -f exprTable.txt -c TRUE -l log2 -z "YlOrRd"

sp_pheatmap.r -f exprTable.txt -A 90 -c TRUE -d TRUE -u exprTable.annorow.txt -v exprTable.annocol.txt
sp_pheatmap.r -f exprTable.txt -A 90 -c TRUE -d TRUE -u exprTable.annorow.txt -v exprTable.annocol.txt -S 'Type=c(TF="red",Enzyme="green"), Count=c("grey","blue")'


sp_pheatmap.r -f exprTable.txt -b "quantile" -i 20
sp_pheatmap.r -f exprTable.txt -b "0,5,10,20,40"  -z "YlGnBu"


splot_pheatmap.sh -f exprTable.txt -A 90 -c TRUE -d TRUE
splot_pheatmap.sh -f exprTable.txt -z Set2
splot_pheatmap.sh -f exprTable.txt -z "green,red" -s row
splot_pheatmap.sh -f exprTable.txt -c TRUE -l log2 -z "YlOrRd"

splot_pheatmap.sh -f exprTable.txt -A 90 -c TRUE -d TRUE -u exprTable.annorow.txt -v exprTable.annocol.txt
splot_pheatmap.sh -f exprTable.txt -A 90 -c TRUE -d TRUE -u exprTable.annorow.txt -v exprTable.annocol.txt -S 'Type=c(TF=\"red\",Enzyme=\"green\"), Count=c(\"grey\",\"blue\")'


splot_pheatmap.sh -f exprTable.txt -b "quantile" -i 20
splot_pheatmap.sh -f exprTable.txt -b "0,5,10,20,40"  -z "YlGnBu"

# Plot.Rmd中用对 sp_volcano_plot 函数测试了 4 次，这里分别用 R 和 Bash 脚本基于相同
# 的数据和参数测试 4 次。

sp_volcano.r -f volcano.txt -l log2FoldChange -d padj -s level

sp_volcano.r -f volcano.txt -l log2FoldChange -d padj -S "0.05,1" -p 'red,blue,black'

sp_volcano.r -f volcano.txt -l log2FoldChange -d padj -S "0.05,1" -p 'red,blue,black' -c TRUE

sp_volcano.r -f volcano.txt -l log2FoldChange -d padj -s level -P Symbol

# 目前的测试是
# transferRscriptToBashScript.py -i sp_volcano.r >splot_volcano.sh 可以直接转换 R 脚本为所需的
# bash 脚本
# 下面的测试可以运行成功

splot_volcano.sh -f volcano.txt -l log2FoldChange -d padj -s level

splot_volcano.sh -f volcano.txt -l log2FoldChange -d padj -S "0.05,1" -p 'red,blue,black'

splot_volcano.sh -f volcano.txt -l log2FoldChange -d padj -S "0.05,1" -p 'red,blue,black' -c TRUE

splot_volcano.sh -f volcano.txt -l log2FoldChange -d padj -s level -P Symbol


splot_venn2.sh -f vennDiagram.data -a Gene -b Sample

splot_venn2.sh -f vennDiagram.data -a Gene -b Sample -c "Set1, Set2,Set3"

sp_enrichment.R -f enrichment.data -x "SampleGroup" -y "Description" -c "Qvalue" -l "Qvalue"  -s "Count"
sp_enrichment.R -f enrichment.data -x "GeneRatio" -y "Description" -c "Qvalue" -l "Qvalue" -q "Count" -r "SampleGroup" -t "Demo1 title"
sp_enrichment.R -f goeast.enrich.txt -x log_odds_ratio -y Term -c p -l p -q q -s q -r Ontology -p right -a 0 -C FALSE -i FALSE -v "Pastel1" -t "Demo2 title" -w 25 -W 25

splot_enrichment.sh -f enrichment.data -x "SampleGroup" -y "Description" -c "Qvalue" -l "Qvalue"  -s "Count"
splot_enrichment.sh -f enrichment.data -x "GeneRatio" -y "Description" -c "Qvalue" -l "Qvalue" -q "Count" -r "SampleGroup" -t "Demo1 title"
splot_enrichment.sh -f goeast.enrich.txt -x log_odds_ratio -y Term -c p -l p -q q -s q -r Ontology -p right -a 0 -C FALSE -i FALSE -v "Pastel1" -t "Demo2 title" -w 25 -W 25


sp_upsetview.R -f upset.wide.data -v 0
sp_upsetview.R -f vennDiagram.data -v 2

splot_upsetview.sh -f upset.wide.data -v 0
splot_upsetview.sh -f vennDiagram.data -v 2

