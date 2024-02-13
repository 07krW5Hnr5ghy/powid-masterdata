package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISize;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("size")
@AllArgsConstructor
public class SizeController {

    private final ISize iSize;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("sizeType") String sizeType,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSize.save(name, sizeType, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "sizes", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,
            @RequestParam("sizeType") String sizeType,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSize.saveAll(names, sizeType, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iSize.delete(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<SizeDTO>> listSize() throws BadRequestExceptions {
        List<SizeDTO> result = iSize.listSize();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "list-size")
    public ResponseEntity<Page<SizeDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<SizeDTO> result = iSize.list(name, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "status-false")
    public ResponseEntity<Page<SizeDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<SizeDTO> result = iSize.listStatusFalse(name, user, sort, sortColumn,
                pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "size-type")
    public ResponseEntity<List<SizeDTO>> findAllSizeTypeName(
            @RequestParam("nameSizeType") String nameSizeType) throws BadRequestExceptions {
        List<SizeDTO> result = iSize.findAllSizeTypeName(nameSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
