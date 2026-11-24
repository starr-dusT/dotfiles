# ROCKNIX on GKD Pixel 2
> Not NixOS? RIP...

## Install

1. Download latest ROCKNIX release from [here](https://github.com/ROCKNIX/distribution/releases)

2. Extract the archived image file

3. Flash ROCKNIX image to sd card with Gnome Disks or similar
    
4. Put SD card into handheld and perform initial installation

5. Enable USB data transfer with:
    - `Settings -> Network Settings -> USB Gadget Function -> FILE_TRANSFER`

6. Download latest Grout release for ROCKNIX [here](https://github.com/rommapp/grout/releases). Extract the contents to `Roms/ports`

7. Copy over theme files from `files/themes` and `logo.png`.

8. Reboot the device and plug in an external USB Wifi dongle:
    - `Settings -> Network Settings -> USB Gadget Function -> Network`

9. Connect console to Wifi and perform initial setup of Grout. Create token for console [here](https://romm.tstarr.us/client-api-tokens). When prompted create directories for relevant systems. Make sure to register device for save sync in `Settings -> Save Sync`.

10. Set settings:
    - `Game Settings -> Save States -> Auto Save/Load -> ON`
    - `Game Settings -> Increment Savestates -> Increment Slot`
    - `Game Settings -> Show Savestate Manager -> Always`
    - `UI Settings -> Select desired theme`
    - `UI Settings -> Show Video Previews`
    - `UI Settings -> Show Favorites On Top`
    - `Game Collection Settings -> Start on System -> Favorites`
    - `Game Collection Settings -> Systems Displayed -> Disable Tools and Music`
    - `System Settings -> Timezone -> America/Los_Angeles`
    - `Network Settings -> Hostname -> osprey`
    
11. Download roms and bios files with Grout

12. Scrape Artwork with Scraper:
    - `Scraper -> Username`
    - `Scraper -> Password`
    - `Scraper -> Scrape Now -> Start`
        
13. Remove Wifi dongle, disable Wifi, and set:
    - `Settings -> Network Settings -> USB Gadget Function -> FILE_TRANSFER`

14. Enjoy!
