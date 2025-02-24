package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestUnit;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.UnitDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUnit;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("unit")
@AllArgsConstructor
public class UnitController {

    private final IUnit iUnit;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:UNIT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestUnit requestUnit,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iUnit.saveAsync(requestUnit, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:UNIT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("unitType") String unitType,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iUnit.delete(name,unitType, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:UNIT_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("unitType") String unitType,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iUnit.activate(name,unitType, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:UNIT_GET')")
    public ResponseEntity<List<UnitDTO>> list() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<UnitDTO>> result = iUnit.listUnit();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("unit-type")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:UNIT_GET')")
    public ResponseEntity<List<UnitDTO>> list(
            @RequestParam("unitType") String unitType
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<UnitDTO>> result = iUnit.listUnitByType(unitType);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:UNIT_GET')")
    public ResponseEntity<List<UnitDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<UnitDTO>> result = iUnit.listFilter();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
