package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IColor;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/color")
@AllArgsConstructor
public class ColorController {
    private final IColor iColor;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iColor.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/colors", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iColor.saveAll(names, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ColorDTO> update(
            @RequestBody() RequestColor requestColor) throws BadRequestExceptions {
        ColorDTO result = iColor.update(requestColor);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iColor.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "list-color")
    public ResponseEntity<List<ColorDTO>> listColor() throws BadRequestExceptions {
        List<ColorDTO> result = iColor.listColor();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    // @PreAuthorize("hasAuthority('AUTH_ROLE:ADMINISTRATOR') and
    // hasAuthority('AUTH_ACCESS:GET_ALL')")
    @GetMapping()
    public ResponseEntity<Page<ColorDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<ColorDTO> result = iColor.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "status-false")
    public ResponseEntity<Page<ColorDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<ColorDTO> result = iColor.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<ColorDTO> findByCode(
            @RequestParam("code") Long code) throws BadRequestExceptions {
        ColorDTO result = iColor.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
