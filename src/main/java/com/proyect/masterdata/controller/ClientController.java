package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ClientDTO;
import com.proyect.masterdata.dto.request.RequestClient;
import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IClient;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/client")
@AllArgsConstructor
public class ClientController {
    private IClient iClient;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestClientSave requestClientSave,
            @RequestParam() String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iClient.save(requestClientSave,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "clients",consumes=MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestClientSave> requestClientSaveList,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iClient.saveAll(requestClientSaveList,user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClientDTO> update(
            @RequestBody() RequestClient requestClient
    ) throws BadRequestExceptions {
        ClientDTO result = iClient.update(requestClient);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("ruc") String ruc,
            @RequestParam("user") String user
    ) throws BadRequestExceptions{
        ResponseDelete result = iClient.delete(ruc,user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ClientDTO>> list(
            @RequestParam(value = "ruc",required = false) String ruc,
            @RequestParam(value = "business",required = false) String business,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "status",required = false) Long status,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<ClientDTO> result = iClient.list(ruc,business,user,status,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
