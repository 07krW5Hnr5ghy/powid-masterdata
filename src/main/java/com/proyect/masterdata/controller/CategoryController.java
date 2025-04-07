package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCreateCategory;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICategory;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("category")
@AllArgsConstructor
public class CategoryController {
    private final ICategory iCategory;
    private final IUtil iUtil;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("description") String description,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCategory.saveAsync(name, description, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_PUT')")
    public ResponseEntity<CategoryDTO> update(
            @RequestParam("tokenUser") String tokenUser,
            @RequestBody() RequestCategory requestCategory) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<CategoryDTO> result = iCategory.update(requestCategory, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iCategory.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<List<CategoryDTO>> listCategory() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CategoryDTO>> result = iCategory.listCategory();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "list")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_GET')")
    public ResponseEntity<Page<CategoryDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "updateStartDate",required = false) String uStartDate,
            @RequestParam(value = "updateEndDate",required = false) String uEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize,
            @RequestParam(value = "status",required = false) Boolean status) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        OffsetDateTime updateStartDate = iUtil.parseToOffsetDateTime(uStartDate,true);
        OffsetDateTime updateEndDate = iUtil.parseToOffsetDateTime(uEndDate,false);
        CompletableFuture<Page<CategoryDTO>> result = iCategory.list(
                name,
                user,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                status);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCategory.activate(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("filter")
    public ResponseEntity<List<CategoryDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CategoryDTO>> result = iCategory.listFilter();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
