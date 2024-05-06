package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IDistrict;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("district")
@AllArgsConstructor
public class DistrictController {
    private final IDistrict iDistrict;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DISTRICT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("province") String province) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDistrict.saveAsync(name, user, province);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping(value = "districts", consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DISTRICT_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("user") String user,
            @RequestParam("codeProvince") String province,
            @RequestBody() List<String> names) throws BadRequestExceptions {
        ResponseSuccess result = iDistrict.saveAll(names, user, province);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DISTRICT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("user") String user) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iDistrict.delete(name, user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "/list-district")
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DISTRICT_GET')")
    public ResponseEntity<List<DistrictDTO>> listProvince() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<DistrictDTO>> result = iDistrict.listDistrict();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "list")
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DISTRICT_GET')")
    public ResponseEntity<Page<DistrictDTO>> list(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince,
            @RequestParam("nameProvince") String nameProvince,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DistrictDTO>> result = iDistrict.list(name, user, codeProvince, nameProvince, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "status-false")
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DISTRICT_GET')")
    public ResponseEntity<Page<DistrictDTO>> listStatusFalse(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince,
            @RequestParam("nameProvince") String nameProvince,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DistrictDTO>> result = iDistrict.listStatusFalse(name, user, codeProvince, nameProvince, sort, sortColumn,
                pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "province")
    public ResponseEntity<List<DistrictDTO>> findByProvince(
            @RequestParam("province") String province) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<DistrictDTO>> result = iDistrict.listDistrictByProvince(province);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
