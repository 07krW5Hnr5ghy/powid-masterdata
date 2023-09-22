package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ClientChannelDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mocks.ClientChannelMocks;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/client-channels")
@AllArgsConstructor
public class ClientChannelController {
    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ClientChannelDTO>> listClientChannels(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ClientChannelMocks clientChannelMocks = new ClientChannelMocks();
        List<ClientChannelDTO> clientChannelList = Arrays.asList(clientChannelMocks.getClientChannelList());
        return new ResponseEntity<>(clientChannelList, HttpStatus.OK);
    }
}
