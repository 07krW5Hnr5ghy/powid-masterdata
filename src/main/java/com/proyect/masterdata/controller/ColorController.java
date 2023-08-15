package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.request.RequestCreateColor;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IColor;
import com.proyect.masterdata.services.impl.ColorImpl;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/color")
@AllArgsConstructor
public class ColorController {
    private final IColor iColor;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name, @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iColor.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/colors")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<RequestCreateColor> requestCreateColorList
    ) throws BadRequestExceptions {
        ResponseSuccess result = iColor.saveAll(requestCreateColorList);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<ColorDTO> update(
            @RequestBody() RequestColor requestColor
    ) throws BadRequestExceptions {
        ColorDTO result = iColor.update(requestColor);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iColor.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/colors")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iColor.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<ColorDTO>> list() throws BadRequestExceptions {
        List<ColorDTO> result = iColor.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<ColorDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ColorDTO result = iColor.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<ColorDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        ColorDTO result = iColor.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
