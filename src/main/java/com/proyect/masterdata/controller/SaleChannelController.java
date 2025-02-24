package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISaleChannel;
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
@RequestMapping("sale-channel")
@AllArgsConstructor
public class SaleChannelController {
    private final ISaleChannel iSaleChannel;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SALE_CHANNEL_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSaleChannel.saveAsync(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SALE_CHANNEL_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("user") String user) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iSaleChannel.delete(tokenUser, user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<SaleChannelDTO>> listSaleChannel() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SaleChannelDTO>> result = iSaleChannel.listSaleChannel();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    public ResponseEntity<Page<SaleChannelDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SaleChannelDTO>> result = iSaleChannel.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    public ResponseEntity<Page<SaleChannelDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SaleChannelDTO>> result = iSaleChannel.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SALE_CHANNEL_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("user") String user) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSaleChannel.activate(tokenUser, user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<SaleChannelDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SaleChannelDTO>> result = iSaleChannel.listFilter();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
