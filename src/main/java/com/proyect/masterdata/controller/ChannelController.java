package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ChannelDTO;
import com.proyect.masterdata.dto.ChannelListDTO;
import com.proyect.masterdata.dto.request.RequestChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IChannel;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/channel")
@AllArgsConstructor
public class ChannelController {
    private IChannel iChannel;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestChannelSave requestChannelSave,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iChannel.save(requestChannelSave, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/channels", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestChannelSave> channels,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iChannel.saveAll(channels, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ChannelDTO> update(
            @RequestParam("name") String name,
            @RequestParam("months") Integer months,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ChannelDTO result = iChannel.update(name, months, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iChannel.delete(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<ChannelListDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<ChannelListDTO> result = iChannel.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "status-false")
    public ResponseEntity<Page<ChannelListDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<ChannelListDTO> result = iChannel.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
