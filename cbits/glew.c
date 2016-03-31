#include <GL/glew.h>
#include <stdio.h>
#include "math.h"

void initGlew() {
    glewExperimental = GL_TRUE;
    if(glewInit() != GLEW_OK) {
        printf("Could not init glew.\n");
        /* return -1; */
    }
}
