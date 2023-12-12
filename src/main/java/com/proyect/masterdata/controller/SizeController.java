package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
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
@RequestMapping("/size")
@AllArgsConstructor
public class SizeController {

    private final ISize iSize;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeSizeType") Long codeSizeType) throws BadRequestExceptions {
        ResponseSuccess result = iSize.save(name, user, codeSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/sizes", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,
            @RequestParam("user") String user,
            @RequestParam("codeSizeType") Long codeSizeType) throws BadRequestExceptions {
        ResponseSuccess result = iSize.saveAll(names, user, codeSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SizeDTO> update(
            @RequestBody() RequestSize requestSize) throws BadRequestExceptions {
        SizeDTO result = iSize.update(requestSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iSize.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list-size")
    public ResponseEntity<List<SizeDTO>> listSize() throws BadRequestExceptions {
        List<SizeDTO> result = iSize.listSize();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<SizeDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "codeSizeType", required = false) Long codeSizeType,
            @RequestParam(value = "nameSizeType", required = false) String nameSizeType,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<SizeDTO> result = iSize.list(name, user, codeSizeType, nameSizeType, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/status-false")
    public ResponseEntity<Page<SizeDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "codeSizeType", required = false) Long codeSizeType,
            @RequestParam(value = "nameSizeType", required = false) String nameSizeType,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<SizeDTO> result = iSize.listStatusFalse(name, user, codeSizeType, nameSizeType, sort, sortColumn,
                pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<SizeDTO> findByCode(
            @RequestParam("code") Long code) throws BadRequestExceptions {
        SizeDTO result = iSize.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/size-type/code")
    public ResponseEntity<List<SizeDTO>> findAllSizeTypeId(
            @RequestParam("codeSizeType") Long codeSizeType) throws BadRequestExceptions {
        List<SizeDTO> result = iSize.findAllSizeTypeId(codeSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/size-type/name")
    public ResponseEntity<List<SizeDTO>> findAllSizeTypeName(
            @RequestParam("nameSizeType") String nameSizeType) throws BadRequestExceptions {
        List<SizeDTO> result = iSize.findAllSizeTypeName(nameSizeType);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
