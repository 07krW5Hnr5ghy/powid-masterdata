package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICountry;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("country")
@AllArgsConstructor
public class CountryController {
    private final ICountry iCountry;
    @GetMapping()
    public ResponseEntity<Page<CountryDTO>> listCountry(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<CountryDTO>> result = iCountry.listCountry(name,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<CountryDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CountryDTO>> result = iCountry.listFilter();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
