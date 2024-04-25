#pragma once
const u_int8_t START_RUN = 0xff; // $ff <count> <value>
const u_int8_t START_SIM = 0xfe; // $fe <count> <bytes back>

void crunch_screen();

void output_run(u_int16_t runlen, u_int8_t ch, u_int16_t i);
