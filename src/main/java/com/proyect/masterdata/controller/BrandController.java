package com.proyect.masterdata.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IBrand;
import org.springframework.web.bind.annotation.*;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import lombok.AllArgsConstructor;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("brand")
@AllArgsConstructor
public class BrandController {

    private final IBrand iBrand;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iBrand.save(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "brands", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> namesList,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iBrand.saveAll(namesList, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iBrand.delete(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:BRAND_GET')")
    public ResponseEntity<Page<BrandDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<BrandDTO> result = iBrand.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:BRAND_GET')")
    public ResponseEntity<Page<BrandDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<BrandDTO> result = iBrand.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(value = "activate",consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iBrand.activate(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
