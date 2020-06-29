
---
title: "Taller 4: Análisis Multivariado"
subtitle: "Análisis de conglomerados (Clusters)"
author: "Julián Camilo Riaño Moreno"
date: "viernes, junio 26, 2020"
output:
  html_document: 
    keep_md: true
    toc: yes
    toc_float: true
    code_folding: hide
  keep_tex: yes
  word_document: default
  pdf_document: 
    keep_tex: yes
    toc: yes
    toc_depth: 3
  fig_cap: yes
header-includes:
- \usepackage{float}
- \floatplacement{figure}{H}
---








<table>
<caption>My table</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">casa</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> V1 </th>
   <th style="text-align:right;"> V2 </th>
   <th style="text-align:right;"> V3 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fixed.acidity_mean </td>
   <td style="text-align:right;"> 6.7679012 </td>
   <td style="text-align:right;"> 6.8740741 </td>
   <td style="text-align:right;"> 6.6037037 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> volatile.acidity_mean </td>
   <td style="text-align:right;"> 0.2854938 </td>
   <td style="text-align:right;"> 0.2940741 </td>
   <td style="text-align:right;"> 0.2729012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> citric.acid_mean </td>
   <td style="text-align:right;"> 0.3448148 </td>
   <td style="text-align:right;"> 0.3544444 </td>
   <td style="text-align:right;"> 0.3380247 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> residual.sugar_mean </td>
   <td style="text-align:right;"> 3.2234568 </td>
   <td style="text-align:right;"> 11.1425926 </td>
   <td style="text-align:right;"> 17.4358025 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> chlorides_mean </td>
   <td style="text-align:right;"> 0.0439012 </td>
   <td style="text-align:right;"> 0.0466296 </td>
   <td style="text-align:right;"> 0.0487407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> free.sulfur.dioxide_mean </td>
   <td style="text-align:right;"> 33.6049383 </td>
   <td style="text-align:right;"> 44.2098765 </td>
   <td style="text-align:right;"> 40.4259259 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> total.sulfur.dioxide_mean </td>
   <td style="text-align:right;"> 128.8024691 </td>
   <td style="text-align:right;"> 162.7160494 </td>
   <td style="text-align:right;"> 156.4197531 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> density_mean </td>
   <td style="text-align:right;"> 0.9920985 </td>
   <td style="text-align:right;"> 0.9963235 </td>
   <td style="text-align:right;"> 0.9990773 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pH_mean </td>
   <td style="text-align:right;"> 3.1974074 </td>
   <td style="text-align:right;"> 3.1741975 </td>
   <td style="text-align:right;"> 3.1498765 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sulphates_mean </td>
   <td style="text-align:right;"> 0.4971605 </td>
   <td style="text-align:right;"> 0.4675309 </td>
   <td style="text-align:right;"> 0.4788889 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> alcohol_mean </td>
   <td style="text-align:right;"> 11.1388889 </td>
   <td style="text-align:right;"> 10.0041152 </td>
   <td style="text-align:right;"> 9.2660494 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> quality_mean </td>
   <td style="text-align:right;"> 6.0864198 </td>
   <td style="text-align:right;"> 5.7160494 </td>
   <td style="text-align:right;"> 5.4938272 </td>
  </tr>
</tbody>
</table>

