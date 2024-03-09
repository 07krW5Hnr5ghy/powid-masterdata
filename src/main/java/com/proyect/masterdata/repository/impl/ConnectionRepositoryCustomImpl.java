package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Connection;
import com.proyect.masterdata.repository.ConnectionRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
@Repository
public class ConnectionRepositoryCustomImpl implements ConnectionRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Connection> searchForConnection(String url, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Connection> criteriaQuery = criteriaBuilder.createQuery(Connection.class);
        Root<Connection> itemRoot = criteriaQuery.from(Connection.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(url,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> connectionList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                connectionList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                connectionList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(connectionList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<Connection> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(url,status);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }
    public List<Predicate> predicateConditions(
            String url,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Connection> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(url!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("url")),url.toUpperCase())));
        }

        if(status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Connection> itemRoot
    ){
        List<Order> connectionList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            connectionList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            connectionList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return connectionList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Connection> itemRoot
    ){
        List<Order> connectionList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            connectionList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            connectionList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return connectionList;
    }

    private long getOrderCount(String url,Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Connection> itemRoot = criteriaQuery.from(Connection.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(url,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
