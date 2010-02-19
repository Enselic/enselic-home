/*
gcc `pkg-config --libs --cflags sdl` -g -o main-sdl main-sdl.c
*/

#include <stdlib.h>
#include <SDL.h>

const int screenWidth = 640;
const int screenHeight = 480;

int main(int argc, char **argv) {
    SDL_Surface *screen;
    SDL_Event event;
    SDL_bool done = SDL_FALSE;

    /* Init */
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        exit(1);
    }
    atexit(SDL_Quit);

    if (!(screen = SDL_SetVideoMode(screenWidth, screenHeight, 32,
SDL_SWSURFACE))) {
        exit(2);
    }

    /* Main loop */
    while (!done && SDL_WaitEvent(&event)) {

        switch (event.type) {
        case SDL_MOUSEMOTION:
            printf("Mouse moved by %d,%d to (%d,%d)\n",
                   event.motion.xrel, event.motion.yrel,
                   event.motion.x, event.motion.y);
            break;

        case SDL_MOUSEBUTTONDOWN:
            printf("Mouse button %d pressed at (%d,%d)\n",
                   event.button.button, event.button.x, event.button.y);
            /* Fall through */

        case SDL_QUIT:
            done = SDL_TRUE;
            break;
        }

        SDL_Rect rect = { 0, 0, screenWidth, screenHeight };
        SDL_FillRect(screen, &rect, 0xff00ff);
        SDL_UpdateRect(screen, 0, 0, screenWidth, screenHeight);
    }

    exit(0);
}
