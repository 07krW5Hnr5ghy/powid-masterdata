package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.request.RequestColorSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ColorMapper;
import com.proyect.masterdata.repository.ColorRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IColor;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class ColorImpl implements IColor {
    private final ColorRepository colorRepository;
    private final ColorMapper colorMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        Color color;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            color = colorRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(color!=null){
            throw new BadRequestExceptions(Constants.ErrorColorExists.toUpperCase());
        }

        try {
            colorRepository.save(colorMapper.colorToName(RequestColorSave
                    .builder().name(name.toUpperCase()).user(datauser.getUser().toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions{

        User datauser;
        List<Color> colors;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            colors = colorRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(!colors.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorColorList.toUpperCase());
        }

        try {
            List<RequestColorSave> colorSaves = names.stream().map(data -> RequestColorSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            colorRepository.saveAll(colorMapper.listColorToListName(colorSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ColorDTO update(RequestColor requestColor) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        Color color;

        try{
            datauser = userRepository.findById(requestColor.getUser().toUpperCase()).orElse(null);
            color = colorRepository.findById(requestColor.getCode()).orElse(null);
        }catch(RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(color==null){
            throw new BadRequestExceptions(Constants.ErrorColor.toUpperCase());
        }

        color.setName(requestColor.getName().toUpperCase());
        color.setUser(datauser.getUser().toUpperCase());
        color.setStatus(requestColor.isStatus());
        color.setDateRegistration(new Date(System.currentTimeMillis()));

        try {
            return colorMapper.colorToColorDTO(colorRepository.save(color));
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions,InternalErrorExceptions{

        User datauser;
        Color color;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            color = colorRepository.findById(code).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(color==null){
            throw new BadRequestExceptions(Constants.ErrorColor.toUpperCase());
        }

        try {
            color.setStatus(false);
            color.setDateRegistration(new Date(System.currentTimeMillis()));
            colorRepository.save(color);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<ColorDTO> listColor() throws BadRequestExceptions{
        List<Color> colors = new ArrayList<>();
        try{
            colors = colorRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(colors.isEmpty()){
            return Collections.emptyList();
        }
        return colorMapper.listColorToListColorDTO(colors);
    }

    @Override
    public List<ColorDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return colorMapper.listColorToListColorDTO(colorRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ColorDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return colorMapper.colorToColorDTO(colorRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
