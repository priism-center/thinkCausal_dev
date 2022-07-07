//derived from https://codepen.io/lmeetr/pen/NWPxomj

var Example = {};
var engine
var world
var render
var runner
var canvas = document.getElementById('galton-canvas')

Example.galton = function() {
  var Engine = Matter.Engine,
    Render = Matter.Render,
    Runner = Matter.Runner,
    Composite = Matter.Composite,
    Composites = Matter.Composites,
    Common = Matter.Common,
    MouseConstraint = Matter.MouseConstraint,
    Mouse = Matter.Mouse,
    World = Matter.World,
    Bodies = Matter.Bodies;

  // create engine
  var engine = Engine.create({
      enableSleeping: true
    }),
    world = engine.world;

  // create renderer
  var render = Render.create({
    canvas: canvas,
    engine: engine,
    options: {
      width: 500,
      height: 830,
      wireframes: false,
      background: '#fff'
    }
  });

  Render.run(render);

  // create runner
  var runner = Runner.create();
  Runner.run(runner, engine);

  const size = 4;

  // add bodies
  let total = document.getElementById('input-n-data').value;
  let distMean = document.getElementById('input-mean').value;
  let distSd = document.getElementById('input-sd').value;
  // console.log('n balls ' + total);

  setInterval(() => {
    if (total-- > 0) {
      const circle = Bodies.circle(jStat.normal.sample(250 + +distMean, +distSd), -20, size, {
        friction: 0.001,
        restitution: 0.45,
        density: 0.005,
        frictionAir: 0.03,
        sleepThreshold: 30,
        render: {
          fillStyle: "#7a9e7eff",
          visible: true
        }
      });

      Matter.Events.on(circle, "sleepStart", () => {
        Matter.Body.setStatic(circle, true);
      });
      World.add(world, circle);
    }
  }, 12);

  const pegs = [];
  const spacingY = 35;
  const spacingX = 20;
  var i, j, lastI;
  for (i = 0; i < 13; i++) {
    for (j = 1; j < i; j++) {
      pegs.push(
        Bodies.circle(
          250 + (j * spacingX - i * (spacingX / 2)),
          i * spacingY,
          size,
          {
            isStatic: true,
            render: {
              fillStyle: "black",
              visible: true
            }
          }
        )
      );
    }
    lastI = i;
  }
  for (i = 0; i < 30; i++) {
    World.add(
      world,
      Bodies.rectangle(
        300 - spacingX + (j * spacingX - i * spacingX),
        0.00 * spacingY + 542,//lastI * spacingY + 255,
        size / 2,
        0.00 + 550, //lastI + 550,
        {
          isStatic: true,
          sleepThreshold: 0.0000,
          render: {
            fillStyle: "#cccccc",
            visible: true
          },
          chamfer: {
            radius: [size * 0.4, size * 0.4, 0, 0]
          }
        }
      )
    );
  }
  World.add(
    world,
    Bodies.rectangle(250, lastI * 1.33 * spacingY + 265, 1000, 15, {
      isStatic: true,
      sleepThreshold: 0.00001,
      render: {
        fillStyle: "#737373",
        visible: false
      }
    })
  );

  // World.add(world, pegs);

  return {
    engine: engine,
    runner: runner,
    render: render,
    canvas: render.canvas,
    stop: function() {
      Matter.Render.stop(render);
      Matter.Runner.stop(runner);
      Matter.Engine.clear(engine);
      Matter.World.clear(world);
      //render.canvas.remove();
    }
  };
};

var galton = Example.galton();

function resetGalton(){
  //console.log('resetting galton')
  galton.stop();
  galton = Example.galton();
}

document.getElementById('reset-galton').addEventListener('click', resetGalton);
document.getElementById('input-n-data').addEventListener('input', resetGalton);
document.getElementById('input-mean').addEventListener('input', resetGalton);
document.getElementById('input-sd').addEventListener('input', resetGalton);
