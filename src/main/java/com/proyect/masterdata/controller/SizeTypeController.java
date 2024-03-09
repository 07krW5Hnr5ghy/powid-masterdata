package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISizeType;
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
@RequestMapping("size-type")
@AllArgsConstructor
public class SizeTypeController {
    private final ISizeType iSizeType;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SIZE_TYPE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSizeType.save(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "size-types", consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SIZE_TYPE_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> names,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSizeType.saveAll(names, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SIZE_TYPE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iSizeType.delete(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_TYPE_GET')")
    public ResponseEntity<List<SizeTypeDTO>> listSizeType() throws BadRequestExceptions {
        List<SizeTypeDTO> result = iSizeType.listSizeType();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "list")
    @PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_TYPE_GET')")
    public ResponseEntity<Page<SizeTypeDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<SizeTypeDTO> result = iSizeType.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "status-false")
    @PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_TYPE_GET')")
    public ResponseEntity<Page<SizeTypeDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<SizeTypeDTO> result = iSizeType.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
