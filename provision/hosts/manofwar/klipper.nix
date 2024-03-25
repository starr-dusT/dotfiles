{ config, lib, pkgs, user, ... }:
  services.klipper = {
    enable = true;
    firmwares = {
      mcu = {
        enable = false;
        # Run klipper-genconf to generate this
        #configFile = ./avr.cfg;
        # Serial port connected to the microcontroller
        #serial = "/dev/serial/by-id/usb-Arduino__www.arduino.cc__0042_55639303235351D01152-if00";
       };
    };
  }
}
