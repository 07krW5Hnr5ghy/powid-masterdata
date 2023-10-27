package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ClientChannelDTO;
import com.proyect.masterdata.dto.request.RequestClientChannel;
import com.proyect.masterdata.dto.request.RequestClientChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IClientChannel;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/client-channel")
@AllArgsConstructor
public class ClientChannelController {
    private IClientChannel iClientChannel;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("ruc") String ruc,
            @RequestBody() RequestClientChannelSave requestClientChannelSave,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iClientChannel.save(ruc,requestClientChannelSave,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/client-channels",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("ruc") String ruc,
            @RequestBody() List<RequestClientChannelSave> clientChannels,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iClientChannel.saveAll(ruc,clientChannels,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClientChannelDTO> update(
            @RequestBody() RequestClientChannel requestClientChannel
    ) throws BadRequestExceptions {
        ClientChannelDTO result = iClientChannel.update(requestClientChannel);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iClientChannel.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ClientChannelDTO>> listModule() throws BadRequestExceptions {
        List<ClientChannelDTO> result = iClientChannel.listClientChannel();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ClientChannelDTO>> list(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<ClientChannelDTO> result = iClientChannel.list(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ClientChannelDTO>> listStatusFalse(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<ClientChannelDTO> result = iClientChannel.listStatusFalse(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClientChannelDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ClientChannelDTO result = iClientChannel.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
