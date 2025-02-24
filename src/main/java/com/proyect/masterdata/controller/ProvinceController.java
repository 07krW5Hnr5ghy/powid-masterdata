package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IProvince;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("province")
@AllArgsConstructor
public class ProvinceController {
    private IProvince iProvince;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("department") String department) throws BadRequestExceptions {
        ResponseSuccess result = iProvince.save(name, tokenUser, department);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iProvince.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iProvince.activate(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_GET')")
    public ResponseEntity<List<ProvinceDTO>> listProvince() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProvinceDTO>> result = iProvince.listProvince();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "list")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_GET')")
    public ResponseEntity<Page<ProvinceDTO>> list(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") UUID codeDepartment,
            @RequestParam("nameDepartment") String nameDepartment,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ProvinceDTO>> result = iProvince.list(name, user, codeDepartment, nameDepartment, sort, sortColumn,
                pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "status-false")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_GET')")
    public ResponseEntity<Page<ProvinceDTO>> listStatusFalse(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") UUID codeDepartment,
            @RequestParam("nameDepartment") String nameDepartment,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ProvinceDTO>> result = iProvince.listStatusFalse(name, user, codeDepartment, nameDepartment, sort,
                sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "department")
    public ResponseEntity<List<ProvinceDTO>> findByDepartment(
            @RequestParam("department") String department) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProvinceDTO>> result = iProvince.listProvinceByDepartment(department);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PROVINCE_GET')")
    public ResponseEntity<List<ProvinceDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProvinceDTO>> result = iProvince.listFilter();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
