"use strict";

function renderLogo(config) {
    const model = config.model;

    // Based on <https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Tutorial/Creating_3D_objects_using_WebGL>
    function initBuffers(gl) {
      const positionBuffer = initPositionBuffer(gl);
      const colorBuffer = initColorBuffer(gl);
      const indexBuffer = initIndexBuffer(gl);
      return {
        position: positionBuffer,
        color: colorBuffer,
        indices: indexBuffer,
      };
    }

    function initPositionBuffer(gl) {
      const positionBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      const positions = [].concat(...model.points.map(function(pc) {return pc[0]}));
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);
      return positionBuffer;
    }

    function initColorBuffer(gl) {
      const colors = [].concat(...model.points.map(function(pc) {return pc[1]}));
      const colorBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(colors), gl.STATIC_DRAW);
      return colorBuffer;
    }

    function initIndexBuffer(gl) {
      const indices = [].concat(...model.indices);
      const indexBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
      gl.bufferData(
        gl.ELEMENT_ARRAY_BUFFER,
        new Uint16Array(indices),
        gl.STATIC_DRAW
      );
      return indexBuffer;
    }

    function drawScene(gl, programInfo, cubeRotation) {
      gl.clearColor(0.0, 0.0, 0.0, 0.0); // Clear to black, fully opaque
      gl.clearDepth(1.0); // Clear everything
      gl.enable(gl.DEPTH_TEST); // Enable depth testing
      gl.depthFunc(gl.LEQUAL); // Near things obscure far things
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

      const aspect = gl.canvas.clientWidth / gl.canvas.clientHeight;
      const zNear = 0.1;
      const zFar = 100.0;
      const projectionMatrix = mat4.create();
      const zoom = 1.1;
      mat4.ortho(projectionMatrix, -aspect / zoom, aspect / zoom, -1 / zoom, 1 / zoom, zNear, zFar);

      const modelViewMatrix = mat4.create();

      // Now move the drawing position a bit to where we want to
      // start drawing the cube.
      mat4.translate(
        modelViewMatrix, // destination matrix
        modelViewMatrix, // matrix to translate
        [-0.0, 0.0, -3.0]
      ); // amount to translate

      mat4.rotate(
        modelViewMatrix, // destination matrix
        modelViewMatrix, // matrix to rotate
        Math.PI / 5,
        [1, 0, 0]
      ); // axis to rotate around (X)
      mat4.rotate(
        modelViewMatrix, // destination matrix
        modelViewMatrix, // matrix to rotate
        Math.PI / 4 + cubeRotation,
        [0, 1, 0]
      ); // axis to rotate around (Y)

      mat4.translate(
        modelViewMatrix, // destination matrix
        modelViewMatrix, // matrix to translate
        [-0.5, -0.5, -0.5]
      ); // amount to translate

      setPositionAttribute(gl, programInfo);
      setColorAttribute(gl, programInfo);
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, programInfo.buffers.indices);
      gl.useProgram(programInfo.program);
      gl.uniformMatrix4fv(
        programInfo.uniformLocations.projectionMatrix,
        false,
        projectionMatrix
      );
      gl.uniformMatrix4fv(
        programInfo.uniformLocations.modelViewMatrix,
        false,
        modelViewMatrix
      );
      {
        const vertexCount = model.indices.length;
        const type = gl.UNSIGNED_SHORT;
        const offset = 0;
        gl.drawElements(gl.TRIANGLES, vertexCount, type, offset);
      }
    }

    function setPositionAttribute(gl, programInfo) {
      const numComponents = 3;
      const type = gl.FLOAT; // the data in the buffer is 32bit floats
      const normalize = false; // don't normalize
      const stride = 0; // how many bytes to get from one set of values to the next
      // 0 = use type and numComponents above
      const offset = 0; // how many bytes inside the buffer to start from
      gl.bindBuffer(gl.ARRAY_BUFFER, programInfo.buffers.position);
      gl.vertexAttribPointer(
        programInfo.attribLocations.vertexPosition,
        numComponents,
        type,
        normalize,
        stride,
        offset
      );
      gl.enableVertexAttribArray(programInfo.attribLocations.vertexPosition);
    }

    function setColorAttribute(gl, programInfo) {
      const numComponents = 4;
      const type = gl.FLOAT;
      const normalize = false;
      const stride = 0;
      const offset = 0;
      gl.bindBuffer(gl.ARRAY_BUFFER, programInfo.buffers.color);
      gl.vertexAttribPointer(
        programInfo.attribLocations.vertexColor,
        numComponents,
        type,
        normalize,
        stride,
        offset
      );
      gl.enableVertexAttribArray(programInfo.attribLocations.vertexColor);
    }


    // Initialize a shader program, so WebGL knows how to draw our data
    function initShaderProgram(gl, vsSource, fsSource) {
      const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vsSource);
      const fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fsSource);

      // Create the shader program
      const shaderProgram = gl.createProgram();
      gl.attachShader(shaderProgram, vertexShader);
      gl.attachShader(shaderProgram, fragmentShader);
      gl.linkProgram(shaderProgram);

      // If creating the shader program failed, alert
      if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
        alert(
          `Unable to initialize the shader program: ${gl.getProgramInfoLog(
            shaderProgram
          )}`
        );
        return null;
      }

      return shaderProgram;
    }

    // creates a shader of the given type, uploads the source and
    // compiles it.
    function loadShader(gl, type, source) {
      const shader = gl.createShader(type);

      // Send the source to the shader object
      gl.shaderSource(shader, source);

      // Compile the shader program
      gl.compileShader(shader);

      // See if it compiled successfully
      if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        alert(
          `An error occurred compiling the shaders: ${gl.getShaderInfoLog(shader)}`
        );
        gl.deleteShader(shader);
        return null;
      }

      return shader;
    }

    function maximizeCanvas(canvas) {
        let rect = canvas.parentElement.getClientRects()[0];
        canvas.width = rect.width;
        canvas.height = rect.width;
    }

    function main() {
      let cubeRotation = 0.0;
      let deltaTime = 0;

      let canvas = config.canvas;
      if (!canvas) {
        canvas = document.getElementById(config.canvasId);
      }
      if (config.resizeCanvas) {
        maximizeCanvas(canvas);
      }
      const gl = canvas.getContext("webgl");

      if (gl === null) {
        console.log("Unable to initialize WebGL. Your browser or machine may not support it.");
        // TODO: fall back to SVG?
        return;
      }

      gl.clearColor(0.0, 0.0, 0.0, 0.0);
      gl.clear(gl.COLOR_BUFFER_BIT);

      // Vertex shader program
      const vsSource = `
        attribute vec4 aVertexPosition;
        attribute vec4 aVertexColor;

        uniform mat4 uModelViewMatrix;
        uniform mat4 uProjectionMatrix;

        varying lowp vec4 vColor;

        void main(void) {
          gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
          vColor = aVertexColor;
        }
      `;

      // Fragment shader program
      const fsSource = `
        varying lowp vec4 vColor;

        void main(void) {
          gl_FragColor = vColor;
        }
      `;

      const shaderProgram = initShaderProgram(gl, vsSource, fsSource);
      const programInfo = {
        buffers: initBuffers(gl),
        program: shaderProgram,
        attribLocations: {
          vertexPosition: gl.getAttribLocation(shaderProgram, "aVertexPosition"),
          vertexColor: gl.getAttribLocation(shaderProgram, "aVertexColor"),
        },
        uniformLocations: {
          projectionMatrix: gl.getUniformLocation(
            shaderProgram,
            "uProjectionMatrix"
          ),
          modelViewMatrix: gl.getUniformLocation(shaderProgram, "uModelViewMatrix"),
        },
      };

      let then = 0;
      let playing = false;
      function startAnimation() {
          if(!playing) {
              playing = true;
          }
      }
      canvas.onclick = startAnimation;
      document.addEventListener("scroll", startAnimation);

      // Draw the scene repeatedly
      function render(now) {
        now *= 0.001; // convert to seconds
        deltaTime = now - then;
        then = now;

        drawScene(gl, programInfo, cubeRotation);
        if(config.animate && playing) {
            cubeRotation += deltaTime;
            if(cubeRotation >= 2 * Math.PI) {
                playing = false;
                cubeRotation = 0;
            }
        }

        requestAnimationFrame(render);
      }
      requestAnimationFrame(render);
    }

    return main();
}
