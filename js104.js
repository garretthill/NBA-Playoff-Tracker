


  const canvasEl = document.querySelector('#canvas');

  const w = canvasEl.width = window.innerWidth;
  const h = canvasEl.height = window.innerHeight * 2;
  
  function loop() {
    requestAnimationFrame(loop);
      ctx.clearRect(0,0,w,h);
    
    confs.forEach((conf) => {
      conf.update();
      conf.draw();
    })
  }
  
  function Confetti () {
    //construct confetti
    const colours = ['#fde132', '#009bde', '#ff6b00'];
    
    this.x = Math.round(Math.random() * w);
    this.y = Math.round(Math.random() * h)-(h/2);
    this.rotation = Math.random()*360;
  
    const size = Math.random()*(w/60);
    this.size = size < 15 ? 15 : size;
  
    this.color = colours[Math.floor(colours.length * Math.random())];
  
    this.speed = this.size/7;
    
    this.opacity = Math.random();
  
    this.shiftDirection = Math.random() > 0.5 ? 1 : -1;
  }
  
  Confetti.prototype.border = function() {
    if (this.y >= h) {
      this.y = h;
    }
  }
  
  Confetti.prototype.update = function() {
    this.y += this.speed;
    
    if (this.y <= h) {
      this.x += this.shiftDirection/3;
      this.rotation += this.shiftDirection*this.speed/100;
    }
  
    if (this.y > h) this.border();
  };
  
  Confetti.prototype.draw = function() {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.size, this.rotation, this.rotation+(Math.PI/2));
    ctx.lineTo(this.x, this.y);
    ctx.closePath();
    ctx.globalAlpha = this.opacity;
    ctx.fillStyle = this.color;
    ctx.fill();
  };
  
  const ctx = canvasEl.getContext('2d');
  const confNum = Math.floor(w / 4);
  const confs = new Array(confNum).fill().map(_ => new Confetti());
  
  loop();

  function myFunctioneast() {
    document.getElementById("divisions-page-east").style.display = "inherit";
    document.getElementById("front_page").style.display = "none";
    
  }

  function myFunctionwest() {
    document.getElementById("divisions-page-west").style.display = "inherit";
    document.getElementById("front_page").style.display = "none";
    
  }

 

  function myFunction2() {
    document.getElementById("game-prediction").style.display = "flex";
  }
  
  
  function myFunctionEastDivisions() {
    document.getElementById("flexcontainereastDivisions").style.display = "flex";
  }


  function myFunctionatlantic() {
    document.getElementById("flexcontaineratlantic").style.display = "flex";
  }

  function myFunctioncentral() {
    document.getElementById("flexcontainercentral").style.display = "flex";
  }

  function myFunctionsoutheast() {
    document.getElementById("flexcontainersoutheast").style.display = "flex";
  }


  
  function myFunctionnorthwest() {
    document.getElementById("flexcontainernorthwest").style.display = "flex";
  }

  function myFunctionpacific() {
    document.getElementById("flexcontainerpacific").style.display = "flex";
  }

  function myFunctionsouthwest() {
    document.getElementById("flexcontainersouthwest").style.display = "flex";
  }



  



























