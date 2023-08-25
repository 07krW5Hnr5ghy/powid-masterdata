package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISize;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/size")
@AllArgsConstructor
public class SizeController {

    private final ISize iSize;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeSizeType") Long codeSizeType
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSize.save(name,user,codeSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/sizes")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,
            @RequestParam("user") String user,
            @RequestParam("codeSizeType") Long codeSizeType
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSize.saveAll(names,user,codeSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<SizeDTO> update(
            @RequestBody() RequestSize requestSize
    ) throws BadRequestExceptions {
        SizeDTO result = iSize.update(requestSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iSize.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<SizeDTO>> list() throws BadRequestExceptions {
        List<SizeDTO> result = iSize.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<SizeDTO>> listStatusFalse() throws BadRequestExceptions {
        List<SizeDTO> result = iSize.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<SizeDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        SizeDTO result = iSize.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<SizeDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        SizeDTO result = iSize.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/user")
    public ResponseEntity<List<SizeDTO>> findByUser(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<SizeDTO> result = iSize.findByUser(user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/size-type/code")
    public ResponseEntity<List<SizeDTO>> findAllSizeTypeId(
            @RequestParam("codeSizeType") Long codeSizeType
    ) throws BadRequestExceptions {
        List<SizeDTO> result = iSize.findAllSizeTypeId(codeSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/size-type/name")
    public ResponseEntity<List<SizeDTO>> findAllSizeTypeName(
            @RequestParam("nameSizeType") String nameSizeType
    ) throws BadRequestExceptions {
        List<SizeDTO> result = iSize.findAllSizeTypeName(nameSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
