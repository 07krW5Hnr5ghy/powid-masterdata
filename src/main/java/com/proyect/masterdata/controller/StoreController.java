package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStore;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStore;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/store")
@AllArgsConstructor
public class StoreController {

    private IStore iStore;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestStoreSave requestClientChannelSave,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iStore.save(requestClientChannelSave, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/stores", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("ruc") String ruc,
            @RequestBody() List<RequestStoreSave> clientChannels,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iStore.saveAll(ruc, clientChannels, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<StoreDTO> update(
            @RequestBody() RequestStore requestClientChannel) throws BadRequestExceptions {
        StoreDTO result = iStore.update(requestClientChannel);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iStore.delete(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<StoreDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<StoreDTO> result = iStore.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/status-false")
    public ResponseEntity<Page<StoreDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<StoreDTO> result = iStore.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
