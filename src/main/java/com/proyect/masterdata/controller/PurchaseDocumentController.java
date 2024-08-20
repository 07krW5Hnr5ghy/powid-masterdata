package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchaseDocument;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase-document")
@AllArgsConstructor
public class PurchaseDocumentController {
    private final IPurchaseDocument iPurchaseDocument;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_DOCUMENT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchaseDocument.save(name,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_DOCUMENT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iPurchaseDocument.delete(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_DOCUMENT_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchaseDocument.activate(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_DOCUMENT_DELETE')")
    public ResponseEntity<List<String>> list() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<String>> result = iPurchaseDocument.list();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<String>> listFilter() throws BadRequestExceptions,ExecutionException,InterruptedException {
        CompletableFuture<List<String>> result = iPurchaseDocument.listFilter();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
