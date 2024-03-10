package com.proyect.masterdata.controller;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.request.RequestSupplier;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplier;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("supplier")
@AllArgsConstructor
public class SupplierController {

    private final ISupplier iSupplier;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestSupplier requestSupplier,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSupplier.save(requestSupplier, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "suppliers", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestSupplier> requestSupplierList,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSupplier.saveAll(requestSupplierList, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("ruc") String ruc,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iSupplier.delete(ruc, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:STOCK','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SUPPLIER_GET')")
    public ResponseEntity<Page<SupplierDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "ruc", required = false) String ruc,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<SupplierDTO> result = iSupplier.list(name, ruc, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
