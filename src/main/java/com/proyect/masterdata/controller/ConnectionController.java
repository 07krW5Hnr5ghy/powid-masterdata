package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ConnectionDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IConnection;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/connection")
@AllArgsConstructor
public class ConnectionController {
    private final IConnection iConnection;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("url") String url,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iConnection.save(url,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/connections",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> urls,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iConnection.saveAll(urls,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("url") String type,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iConnection.delete(type,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ConnectionDTO>> list(
            @RequestParam(value = "url",required = false) String url,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<ConnectionDTO> result = iConnection.list(url,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ConnectionDTO>> listStatusFalse(
            @RequestParam(value = "name",required = false) String url,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<ConnectionDTO> result = iConnection.listStatusFalse(url,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
