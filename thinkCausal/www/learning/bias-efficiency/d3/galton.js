//https://codepen.io/lmeetr/pen/NWPxomj

var Example = Example || {};
var engine
var world
var render
var runner

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
    element: document.body,
    engine: engine,
    options: {
      width: 500,
      height: 830,
      wireframes: false
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
  // console.log('n balls ' + total)
  setInterval(() => {
    if (total-- > 0) {
      const circle = Bodies.circle(jStat.normal.sample(250 + +distMean, +distSd), -20, size, {// 250 + (-0.5 + Math.random()), -20, size, {
        friction: 0.00001,
        restitution: 0.5,
        density: 0.001,
        frictionAir: 0.042,
        sleepThreshold: 25,
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
  }, 10);

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
              fillStyle: "#ffffff",
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
        lastI * spacingY + 255,
        size / 2,
        lastI + 800,
        {
          isStatic: true,
          render: {
            fillStyle: "#ffffff",
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
    Bodies.rectangle(250, lastI * 1.33 * spacingY + 257, 1000, 50, {
      isStatic: true,
      render: {
        fillStyle: "#ffffff",
        visible: true
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
      render.canvas.remove();
    }
  };
};

var galton = Example.galton();

document.getElementById('reset-galton').addEventListener('click', event => {
  console.log(galton)
  galton.stop();
  galton = Example.galton();
});
