!function(){function e(){"undefined"!=typeof window.sessionStorage&&(sessionStorage.setItem("docs-search-query",L.query),sessionStorage.setItem("docs-search-index",L.selectedIndex))}function t(t){if(40===t.keyCode)L.selectedIndex<g-1&&(L.selectedIndex+=1,e(),o()),t.preventDefault();else if(38===t.keyCode)L.selectedIndex>0&&(L.selectedIndex-=1,e(),o()),t.preventDefault();else if(13===t.keyCode){var n=f.querySelector("li.selected a");n&&(a.dispatchEvent(new Event("openSearchResult")),i(),page.openURL(n),t.preventDefault())}}function n(){m||(m=!0,document.addEventListener("keydown",t),overlayController.show(i),p.style.visibility="visible",dynamics.animate(p,{opacity:1},{type:dynamics.easeInOut,friction:350,duration:250}),L.selectedIndex=0,e(),o())}function i(){m&&(m=!1,document.removeEventListener("keydown",t),overlayController.hide(),dynamics.animate(p,{opacity:0},{type:dynamics.easeInOut,duration:250,complete:function(){p.style.visibility="hidden"}}))}function r(t,n){if(t!==x){if(/[0-9-\s]{12,20}/.test(t)||t.indexOf("sk_")>-1)return g=0,void s([]);x=t,l.search(t,{attributesToHighlight:"hierarchy,content",highlightPreTag:"[h]",highlightPostTag:"[/h]",hitsPerPage:10},function(t,i){if(!t){var r={},o=i.hits.filter(function(e){var t=parseURL(e.url);return r[t.pathname]=!0,!0}),c=["recipes","more recipes","next steps","next up","up next"],d=o.map(function(e){for(var t=Object.keys(e._highlightResult.hierarchy).sort(),n="",i="",r=t.length-1;r>0;r--){var o=e._highlightResult.hierarchy[t[r]];o&&(o=o.value),o&&i!==o&&-1===c.indexOf(o.toLowerCase())&&(n.length>0&&(n=" \u203a "+n),n=o+n,i=o)}var s=parseURL(e.url);return{title:n,url:s.pathname+("#content"===s.hash?"":s.hash),content:e._highlightResult.content&&e._highlightResult.content.value,type:"page",declaration:""}});n||(L.selectedIndex=0,e()),s(d)}})}}function o(){var e=f.querySelector("li.selected")||f.querySelector("li:first-child");if(e){e.classList.remove("selected"),e=f.querySelector("li:nth-child("+(L.selectedIndex+1)+")"),e.classList.add("selected");var t=e.getBoundingClientRect(),n=t.top-12,i=t.top+t.height+12;0===L.selectedIndex?document.body.scrollTop=0:n<0?document.body.scrollTop=document.body.scrollTop+n:i>=window.innerHeight&&(document.body.scrollTop=document.body.scrollTop-(window.innerHeight-i))}}function c(e){if("function"==typeof DOMParser){e=(new DOMParser).parseFromString(e,"text/html").documentElement.textContent}else e=$("<div/>").text(e).html();return e=e.replace(/\[h\]/g,"<em>"),e=e.replace(/\[\/h\]/g,"</em>")}function s(e){g=e.length;for(var t=document.createDocumentFragment(),n=0;n<e.length;n++){var r=e[n],o=document.createElement("li");n===L.selectedIndex&&o.classList.add("selected");var s=document.createElement("a");if(s.innerHTML=c(r.title),s.href=r.url,s.addEventListener("click",function(e){i(),page.openLink(e)}),o.appendChild(s),r.declaration){var d=document.createElement("div");d.classList.add("tag"),d.classList.add("method"),d.innerText=r.declaration.replace(/http.*\/v1/,"/v1"),o.appendChild(d)}if("reference"===r.type||-1!==r.url.indexOf("/docs/api")){var d=document.createElement("div");d.classList.add("api-reference"),o.appendChild(d)}if(r.content){var l=document.createElement("p");l.innerHTML=c(r.content),o.appendChild(l)}t.appendChild(o)}f.innerHTML="",f.appendChild(t),y.style.visibility=0===g?"visible":""}var d=algoliasearch("Y4PFTTJ91H","13cb82a358f97cd95f3321f14d6c3e44"),l=d.initIndex("stripe-docs"),a=document.querySelector(".search"),u=a.querySelector("input"),h=a.querySelector(".search-text"),p=document.querySelector(".search-results"),y=document.querySelector(".search-after-text--no-results"),f=p.querySelector(".results"),m=!1,v=debounce(r,100),g=0,x=null,L={query:"",selectedIndex:0};!function(){if("undefined"!=typeof window.sessionStorage){var e=sessionStorage.getItem("docs-search-query"),t=sessionStorage.getItem("docs-search-index")||0;e&&(L.query=e,L.selectedIndex=t,u.value=L.query,h.textContent=L.query,r(L.query,!0)),document.addEventListener("keyup",function(e){if((191==e.keyCode||83==e.keyCode)&&document.activeElement!=u)return u.focus(),u.select(),!1})}}(),u.addEventListener("keyup",function(){var t=u.value;L.query!==t&&(L.query=t,h.textContent=t,y.style.visibility="",e(),t.length>0?(n(),v(t)):i())}),u.addEventListener("focus",function(){u.value.length>0&&n(),u.select()}),f.addEventListener("mousedown",function(t){for(var n=t.target;n&&"LI"!==n.tagName;)n=n.parentNode;if(n){var i=n.parentNode.children;i=Array.prototype.slice.call(i,this),L.selectedIndex=i.indexOf(n),e(),o()}})}();