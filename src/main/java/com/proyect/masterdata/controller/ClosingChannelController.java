package com.proyect.masterdata.controller;

import org.springframework.data.domain.Page;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.ClosingChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IClosingChannel;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("closing-channel")
@AllArgsConstructor
public class ClosingChannelController {

    private final IClosingChannel iClosingChannel;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CLOSING_CHANNEL_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String user) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iClosingChannel.save(name, user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    public ResponseEntity<Page<ClosingChannelDTO>> listClosingChannel(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ClosingChannelDTO>> result = iClosingChannel.listClosingChannel(name, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<ClosingChannelDTO>> list() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ClosingChannelDTO>> result = iClosingChannel.list();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
