---
title: Excursions with plotting big data
tags: prog
toc: y
---

# Tensorfail extraction

* Is tf.js a viable numerical library for extract and cleaning data on browser side?
  * Big no, everything turns into floating points and promise objects.

```js
    let range = n => [...Array(n).keys()]
        const main = async () => {
          const rawdata = await fetch("https://www.bigdata....").then(response => response.text())
          const inter = window.atob(rawdata)
          const jsondata = JSON.parse(inter)["candles"]
          //const yaxis = jsondata.map(x => x["close"])
          //const xaxis = range(yaxis.length)
          
          //tensorfail attempt at extract x and y data
          const tfDF = tf.data.array(jsondata)
          //Even logging the output requires a promise
          await tfDF.take(1).forEachAsync(e => console.log(e))
          //failed to even extract closing price
          const closing = await
            tfDF.mapAsync(x => new Promise(function(resolve){
              setTimeout(() => {
                resolve(x["close"]);
              }, Math.random()*1000 + 500);
            }));

          //adds small floating point error
          const tf_xaxis = await tf.linspace(0,closing.size,closing.size)
          const xaxis = await tf_xaxis.array() //conversion to array is a promise
        }
    

```


# Plotting Competitors

* Echarts
* Chartjs
* uplot

## Simple line graph

* Echarts - pass
* Chartjs - failed
* uplot - pass


```js
    let range = n => [...Array(n).keys()]
        const main = async () => {
          const rawdata = await fetch("https://raw.githubusercontent.com/userJY/stuff/main/example").then(response => response.text())
          const inter = window.atob(rawdata)
          const jsondata = JSON.parse(inter)["candles"]
          const ydata = jsondata.map(x => x["close"])
          const xdata = range(ydata.length)

          
          let chartDom =  document.querySelector('main');
          let myChart =  echarts.init(chartDom);
          let option;

          option =  {
            xAxis: {
              type: 'category',
              data: xdata
            },
            yAxis: {
              type: 'value'
            },
            series: [
              {
                data: ydata,
                type: 'line'
              }
            ],
            tooltip: {
              trigger: 'axis',
              triggerOn: 'mousemove|click',
              axisPointer: {
                type: 'cross',
                animation: false,
                label: {
                  backgroundColor: '#ccc',
                  borderColor: '#aaa',
                  borderWidth: 1,
                  shadowBlur: 0,
                  shadowOffsetX: 0,
                  shadowOffsetY: 0,
                  color: '#222'
                }
              },
              formatter: function (params) {
                //console.log(params)
                return (
                  params[0].name +
                  '<br />' +
                  params[0].value +
                  '%'
                );
              }
            },

          };

 option &&  myChart.setOption(option);

```

```js
