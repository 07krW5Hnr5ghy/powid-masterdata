package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.StateMapper;
import com.proyect.masterdata.repository.StateRepository;
import com.proyect.masterdata.services.IMasterList;
import com.proyect.masterdata.services.IState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StateImpl implements IState {
    private final StateRepository stateRepository;
    private final StateMapper stateMapper;
    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            stateRepository.save(stateMapper.stateToName(name.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
        try {
            stateRepository.saveAll(stateMapper.listStateToListName(
                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public StateDTO update(RequestState requestState) throws BadRequestExceptions {
        try {
            requestState.setName(requestState.getName().toUpperCase());
            State state = stateRepository.save(stateMapper.requestStateToState(requestState));
            return stateMapper.stateToStateDTO(state);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            stateRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            stateRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<StateDTO> list() throws BadRequestExceptions{
        try {
            return stateMapper.listStateToListStateDTO(stateRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public StateDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return stateMapper.stateToStateDTO(stateRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public StateDTO findByName(String name) throws BadRequestExceptions{
        try {
            return stateMapper.stateToStateDTO(stateRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
