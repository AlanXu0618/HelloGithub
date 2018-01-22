CREATE OR REPLACE PACKAGE cux_gl_fin_expenses_pkg IS

  -- Author  : LBY
  -- Created : 2017/6/21 18:52:14
  -- Purpose : ������ó����

  -- Public type declarations
  TYPE exp_dtls_tbl_type IS TABLE OF cux_gl_fin_expense_details%ROWTYPE INDEX BY BINARY_INTEGER;
  -- Public constant declarations
  -- Public variable declarations

  -- Public function and procedure declarations
  FUNCTION get_lookup_meaning(p_lookup_code VARCHAR2,
                              p_lookup_type VARCHAR2,
                              p_language    VARCHAR2 DEFAULT userenv('LANG')) RETURN VARCHAR2;

  FUNCTION get_flex_value_description(p_flex_value_set_name IN VARCHAR2, p_flex_value IN VARCHAR2)
    RETURN VARCHAR2;

  FUNCTION get_error_messages(p_expense_id IN NUMBER) RETURN VARCHAR2;

  FUNCTION get_ou_fincost_center(p_org_id IN NUMBER) RETURN VARCHAR2;

  FUNCTION get_assig_dept_desc(p_org_id IN NUMBER, p_assig_dept_code IN VARCHAR2) RETURN VARCHAR2;

  PROCEDURE submit(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                   p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                   p_expenses_tbl  IN dbms_utility.number_array,
                   x_return_status OUT VARCHAR2,
                   x_msg_count     OUT NUMBER,
                   x_msg_data      OUT VARCHAR2);

  PROCEDURE unsubmit(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                     p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                     p_expense_id    IN NUMBER,
                     x_return_status OUT VARCHAR2,
                     x_msg_count     OUT NUMBER,
                     x_msg_data      OUT VARCHAR2);

  PROCEDURE unsubmit(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                     p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                     p_expenses_tbl  IN dbms_utility.number_array,
                     x_return_status OUT VARCHAR2,
                     x_msg_count     OUT NUMBER,
                     x_msg_data      OUT VARCHAR2);

  PROCEDURE assignment(p_init_msg_list   IN VARCHAR2 DEFAULT fnd_api.g_false,
                       p_commit          IN VARCHAR2 DEFAULT fnd_api.g_true,
                       p_expense_id      IN NUMBER,
                       p_assignment_dept IN VARCHAR2,
                       p_split_amount    IN NUMBER,
                       p_remarks         IN VARCHAR2,
                       x_return_status   OUT VARCHAR2,
                       x_msg_count       OUT NUMBER,
                       x_msg_data        OUT VARCHAR2);

  PROCEDURE unassign(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                     p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                     p_expense_id    IN NUMBER,
                     p_detail_id     IN NUMBER,
                     x_return_status OUT VARCHAR2,
                     x_msg_count     OUT NUMBER,
                     x_msg_data      OUT VARCHAR2);

  PROCEDURE confirm(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                    p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                    p_expense_id    IN NUMBER,
                    p_detail_id     IN NUMBER,
                    x_return_status OUT VARCHAR2,
                    x_msg_count     OUT NUMBER,
                    x_msg_data      OUT VARCHAR2);

  PROCEDURE unconfirm(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                      p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                      p_expense_id    IN NUMBER,
                      p_detail_id     IN NUMBER,
                      x_return_status OUT VARCHAR2,
                      x_msg_count     OUT NUMBER,
                      x_msg_data      OUT VARCHAR2);

  PROCEDURE create_gl_change_status(p_expense_id    IN NUMBER,
                                    p_group_id      IN NUMBER,
                                    x_return_status OUT VARCHAR2);

  PROCEDURE create_gl(p_init_msg_list   IN VARCHAR2 DEFAULT fnd_api.g_false,
                      p_commit          IN VARCHAR2 DEFAULT fnd_api.g_true,
                      p_expense_id      IN NUMBER,
                      p_gl_date         IN DATE,
                      p_department_flag IN VARCHAR2,
                      x_return_status   OUT VARCHAR2,
                      x_msg_count       OUT NUMBER,
                      x_msg_data        OUT VARCHAR2);

  PROCEDURE conc_journal(errbuf       OUT NOCOPY VARCHAR2,
                         retcode      OUT NOCOPY VARCHAR2,
                         p_expense_id IN NUMBER);

  PROCEDURE reversal_to_confim(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                               p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                               p_expense_id    IN NUMBER,
                               x_return_status OUT VARCHAR2,
                               x_msg_count     OUT NUMBER,
                               x_msg_data      OUT VARCHAR2);

  PROCEDURE failed_to_confim(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                             p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                             p_expense_id    IN NUMBER,
                             x_return_status OUT VARCHAR2,
                             x_msg_count     OUT NUMBER,
                             x_msg_data      OUT VARCHAR2);

  PROCEDURE conc_reversal(errbuf   OUT NOCOPY VARCHAR2,
                          retcode  OUT NOCOPY VARCHAR2,
                          p_org_id IN NUMBER DEFAULT NULL);

END cux_gl_fin_expenses_pkg;
/
CREATE OR REPLACE PACKAGE BODY cux_gl_fin_expenses_pkg IS

  -- Private type declarations
  -- Private constant declarations
  g_pkg_name        CONSTANT VARCHAR2(30) := 'cux_gl_fin_expenses_pkg';
  g_conc_request_id CONSTANT NUMBER := fnd_global.conc_request_id;
  -- Private variable declarations
  g_user_je_source_name   VARCHAR2(30) := '�˹�';
  g_user_je_category_name VARCHAR2(30) := '����ƾ֤';
  g_je_line_num           NUMBER;

  -- Function and procedure implementations
  PROCEDURE log(p_text IN VARCHAR2) IS
  BEGIN
    IF g_conc_request_id > 0 THEN
      fnd_file.put_line(fnd_file.log, p_text);
    ELSE
      dbms_output.put_line(p_text);
    END IF;
  END;

  PROCEDURE output(p_text IN VARCHAR2) IS
  BEGIN
    IF g_conc_request_id > 0 THEN
      fnd_file.put_line(fnd_file.output, p_text);
    ELSE
      dbms_output.put_line(p_text);
    END IF;
  END;

  -- Function and procedure implementations
  /* =============================================
  *   PROCEDURE
  *   NAME :
  *         get_lookup_meaning
  *   DESCRIPTION:
  *    -- ��ȡ���ٱ���ĺ��壨����ʧЧֵ��
  *   ARGUMENT:
  *
  *   RETURN:
  *
  *   HISTORY:
  *     
  * =============================================*/
  FUNCTION get_lookup_meaning(p_lookup_code VARCHAR2,
                              p_lookup_type VARCHAR2,
                              p_language    VARCHAR2 DEFAULT userenv('LANG')) RETURN VARCHAR2 IS
    l_meaning VARCHAR2(80);
  BEGIN
    SELECT flv.meaning
      INTO l_meaning
      FROM fnd_lookup_values flv
     WHERE flv.lookup_type = p_lookup_type
       AND flv.lookup_code = p_lookup_code
       AND flv.language = p_language;
    RETURN l_meaning;
  EXCEPTION
    WHEN no_data_found THEN
      RETURN NULL;
  END get_lookup_meaning;

  /* =============================================
  *   PROCEDURE
  *   NAME :
  *         get_flex_value_descption
  *   DESCRIPTION:
  *         --��ȡֵ��ֵ��Ӧ������������ʧЧֵ��
  *   ARGUMENT:
  *
  *   RETURN:
  *
  *   HISTORY:
  *     
  * =============================================*/
  FUNCTION get_flex_value_description(p_flex_value_set_name IN VARCHAR2, p_flex_value IN VARCHAR2)
    RETURN VARCHAR2 IS
    l_return VARCHAR2(240);
    CURSOR c_desc IS
      SELECT ffv.description
        FROM fnd_flex_value_sets f, fnd_flex_values_vl ffv
       WHERE f.flex_value_set_name = p_flex_value_set_name
         AND f.flex_value_set_id = ffv.flex_value_set_id
         AND ffv.flex_value = p_flex_value
         AND f.validation_type = 'I';
  BEGIN
    OPEN c_desc;
    FETCH c_desc
      INTO l_return;
    IF c_desc%NOTFOUND THEN
      l_return := NULL;
    END IF;
    CLOSE c_desc;
    RETURN l_return;
  END get_flex_value_description;

  /* =============================================
  *   PROCEDURE
  *   NAME :
  *         get_error_messages
  *   DESCRIPTION:
  *         --��ȡ��¼�Ĵ�����Ϣ�ı�
  *   ARGUMENT:
  *
  *   RETURN:
  *
  *   HISTORY:
  *     
  * =============================================*/
  FUNCTION get_error_messages(p_expense_id IN NUMBER) RETURN VARCHAR2 IS
    l_error_messages VARCHAR2(2000);
    CURSOR cur_msgs IS
      SELECT a.message_text
        FROM cux_gl_fin_expense_msgs a
       WHERE a.message_type = 'ERROR'
         AND a.entity_code = 'FIN_EXPENSE'
         AND a.entity_id = p_expense_id
       ORDER BY a.transaction_id;
  
  BEGIN
    FOR i IN cur_msgs LOOP
      l_error_messages := l_error_messages || ',' || i.message_text;
    END LOOP;
    l_error_messages := ltrim(l_error_messages, ',');
  
    RETURN l_error_messages;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_error_messages;

  /* =============================================
  *   PROCEDURE
  *   NAME :
  *         get_fincost_center
  *   DESCRIPTION:
  *         -- ��ȡOU��Ӧ�� "�ʽ����"
  *   ARGUMENT:
  *
  *   RETURN:
  *
  *   HISTORY:
  *     
  * =============================================*/
  FUNCTION get_ou_fincost_center(p_org_id IN NUMBER) RETURN VARCHAR2 IS
    l_cost_center_code VARCHAR2(80);
  BEGIN
    SELECT flv.attribute1 cost_center_code
      INTO l_cost_center_code
      FROM fnd_lookup_values flv, hr_operating_units hou
     WHERE flv.lookup_type = 'CUX_FINCOST_CENTER'
       AND flv.view_application_id = 20003
       AND flv.language = userenv('LANG')
       AND hou.short_code = flv.tag
       AND hou.organization_id = p_org_id;
  
    RETURN l_cost_center_code;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_ou_fincost_center;

  /* =============================================
  *   PROCEDURE
  *   NAME :
  *         get_assig_dept_desc
  *   DESCRIPTION:
  *         -- ��ȡ���䲿�ŵ�����
  *   ARGUMENT:
  *
  *   RETURN:
  *
  *   HISTORY:
  *     
  * =============================================*/
  FUNCTION get_assig_dept_desc(p_org_id IN NUMBER, p_assig_dept_code IN VARCHAR2) RETURN VARCHAR2 IS
    l_assig_dept_desc VARCHAR2(240);
  BEGIN
    IF lengthb(p_assig_dept_code) = 7 THEN
      l_assig_dept_desc := cux_common_pkg.get_flex_value_desc('JXCC_COA_CC', p_assig_dept_code);
    ELSE
      SELECT flv.description
        INTO l_assig_dept_desc
        FROM fnd_lookup_values flv, hr_operating_units hou
       WHERE flv.lookup_type = 'CUX_FINCOST_CENTER'
         AND flv.view_application_id = 20003
         AND flv.language = userenv('LANG')
         AND hou.short_code = flv.tag
         AND hou.organization_id = p_org_id
         AND flv.attribute1 = p_assig_dept_code;
    END IF;
  
    RETURN l_assig_dept_desc;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_assig_dept_desc;

  /*==================================================
  Procedure Name:  submit_check
  Description:
      ��������ύ���
  Argument:
      p_expense_id    ��������õ��ݱ�ʶ
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE submit_check(p_expense_id IN NUMBER, x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'submit_check';
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_primary_currency VARCHAR2(15);
    l_return_status    VARCHAR2(1);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    l_return_status := fnd_api.g_ret_sts_success;
  
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all cgfe
     WHERE cgfe.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    BEGIN
      SELECT a.currency_code
        INTO l_primary_currency
        FROM gl_sets_of_books a, hr_operating_units b
       WHERE a.set_of_books_id = b.set_of_books_id
         AND b.organization_id = l_fin_exp_rec.org_id;
    EXCEPTION
      WHEN no_data_found THEN
        fnd_message.set_name('CS', 'CS_COST_GET_CURRENCY_FAILED');
        fnd_msg_pub.add;
        RAISE fnd_api.g_exc_error;
    END;
  
    IF l_fin_exp_rec.expense_date IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '��������');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_fin_exp_rec.drcr_type IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '��/֧');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_fin_exp_rec.expense_type IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '��������');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_fin_exp_rec.expense_item IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '������Ŀ');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_fin_exp_rec.currency_code IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '����');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_fin_exp_rec.expense_amount IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '���ý��');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_fin_exp_rec.bank_account_id IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '�����˺�');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_primary_currency <> l_fin_exp_rec.currency_code AND
       l_fin_exp_rec.currency_code IS NOT NULL THEN
      -- �Ǳ�λ�ң����������������ֶ�
      IF l_fin_exp_rec.conversion_rate_date IS NULL THEN
        fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
        fnd_message.set_token('ATTRIBUTE', '��������');
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      IF l_fin_exp_rec.conversion_type_code IS NULL THEN
        fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
        fnd_message.set_token('ATTRIBUTE', '��������');
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      IF l_fin_exp_rec.conversion_rate IS NULL THEN
        fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
        fnd_message.set_token('ATTRIBUTE', '����');
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    END IF;
  
    IF l_fin_exp_rec.status_code <> 'NEW' THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_SUBMIT');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(l_fin_exp_rec.expense_number));
      fnd_message.set_token('STATUS',
                            cux_fnd_common_utl.get_lookup_meaning(l_fin_exp_rec.status_code,
                                                                  'CUX_FINCOST_TYPE'));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END submit_check;

  PROCEDURE assignment_check(p_fin_exp_rec   IN cux_gl_fin_expenses%ROWTYPE,
                             p_exp_dtls_tbl  IN exp_dtls_tbl_type,
                             x_return_status OUT VARCHAR2);
  PROCEDURE assign_change_status(p_expense_id IN NUMBER, x_return_status OUT VARCHAR2);

  /*==================================================
  Procedure Name:  submit
  Description:
      ��������ύ
  Argument:
      p_expenses_tbl  ��������õ���
      x_return_status : ���ش������
      x_msg_count     : ������Ϣ����
      x_msg_data      ��������Ϣ�ı�
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE submit(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                   p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                   p_expenses_tbl  IN dbms_utility.number_array,
                   x_return_status OUT VARCHAR2,
                   x_msg_count     OUT NUMBER,
                   x_msg_data      OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'submit';
  
    i                  NUMBER;
    j                  NUMBER;
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_tbl     exp_dtls_tbl_type;
    l_limit_amount     NUMBER;
    l_auto_assign_dept VARCHAR2(80);
    l_return_status    VARCHAR2(1);
    l_msg_count        NUMBER;
    l_msg_data         VARCHAR2(2000);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    i := p_expenses_tbl.first;
    WHILE i IS NOT NULL LOOP
      SELECT *
        INTO l_fin_exp_rec
        FROM cux_gl_fin_expenses_all cgfe
       WHERE cgfe.expense_id = p_expenses_tbl(i)
         FOR UPDATE NOWAIT;
    
      SELECT *
        BULK COLLECT
        INTO l_exp_dtls_tbl
        FROM cux_gl_fin_expense_details_all d
       WHERE d.expense_id = p_expenses_tbl(i)
         FOR UPDATE NOWAIT;
    
      submit_check(p_expense_id => p_expenses_tbl(i), x_return_status => l_return_status);
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    
      UPDATE cux_gl_fin_expenses_all cgfe
         SET cgfe.submit_by         = fnd_global.user_id,
             cgfe.submit_date       = SYSDATE,
             cgfe.status_code       = 'UNMATCH',
             cgfe.last_updated_by   = fnd_global.user_id,
             cgfe.last_update_date  = SYSDATE,
             cgfe.last_update_login = fnd_global.login_id
       WHERE cgfe.status_code = 'NEW'
         AND cgfe.expense_id = p_expenses_tbl(i);
    
      UPDATE cux_gl_fin_expense_details cgfed
         SET cgfed.status_code       = 'UNMATCH',
             cgfed.last_updated_by   = fnd_global.user_id,
             cgfed.last_update_date  = SYSDATE,
             cgfed.last_update_login = fnd_global.login_id
       WHERE cgfed.status_code = 'NEW'
         AND cgfed.expense_id = p_expenses_tbl(i);
    
      -- �޶���Χ��ֱ��ȷ��
      BEGIN
        SELECT attribute1
          INTO l_limit_amount
          FROM cux_gl_fincost_map_all m
         WHERE m.enabled_flag = 'Y'
           AND trunc(SYSDATE) BETWEEN nvl(m.start_date_active, trunc(SYSDATE)) AND
               nvl(m.end_date_active, SYSDATE)
           AND m.org_id = l_fin_exp_rec.org_id
           AND m.expense_type = l_fin_exp_rec.expense_type
           AND m.expense_item = l_fin_exp_rec.expense_item
           AND m.drcr_type = l_fin_exp_rec.drcr_type;
      EXCEPTION
        WHEN OTHERS THEN
          l_limit_amount := NULL;
      END;
    
      IF l_fin_exp_rec.expense_amount <= nvl(l_limit_amount, 0) AND l_limit_amount IS NOT NULL THEN
        -- С�����ֱ��ȷ��
        l_auto_assign_dept := get_ou_fincost_center(p_org_id => l_fin_exp_rec.org_id);
        IF l_auto_assign_dept IS NULL THEN
          fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
          fnd_message.set_token('MESSAGE',
                                '����OU���ҿ��룺CUX_FINCOST_CENTER ���õ�ֱ��ȷ�ϳɱ����Ĳ����ڡ�');
          fnd_msg_pub.add;
          RAISE fnd_api.g_exc_error;
        END IF;
      
        -- �Զ��������� & �Զ�ȷ��
        cux_gl_fin_expenses_pkg.assignment(p_init_msg_list   => fnd_api.g_false,
                                           p_commit          => fnd_api.g_false,
                                           p_expense_id      => l_fin_exp_rec.expense_id,
                                           p_assignment_dept => l_auto_assign_dept,
                                           p_split_amount    => l_fin_exp_rec.expense_amount,
                                           p_remarks         => l_fin_exp_rec.remarks,
                                           x_return_status   => l_return_status,
                                           x_msg_count       => l_msg_count,
                                           x_msg_data        => l_msg_data);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      
      ELSE
        ------------------------------------------------------------------------------------------ 
        -- ���¶�ȡ����
        SELECT *
          INTO l_fin_exp_rec
          FROM cux_gl_fin_expenses_all cgfe
         WHERE cgfe.expense_id = p_expenses_tbl(i);
      
        SELECT *
          BULK COLLECT
          INTO l_exp_dtls_tbl
          FROM cux_gl_fin_expense_details_all d
         WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
           AND d.expense_id = p_expenses_tbl(i);
      
        -- ����Ѵ��ڷ�������, ֱ�Ӵ������: �ϴ����ó���
        IF l_exp_dtls_tbl.count > 0 THEN
          assignment_check(p_fin_exp_rec   => l_fin_exp_rec,
                           p_exp_dtls_tbl  => l_exp_dtls_tbl,
                           x_return_status => l_return_status);
          IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
          END IF;
        
          j := l_exp_dtls_tbl.first;
          WHILE j IS NOT NULL LOOP
            UPDATE cux_gl_fin_expense_details d
               SET d.assigned_by       = fnd_global.user_id,
                   d.status_code       = 'MATCHED',
                   d.last_updated_by   = fnd_global.user_id,
                   d.last_update_date  = SYSDATE,
                   d.last_update_login = fnd_global.login_id
             WHERE d.status_code = 'UNMATCH'
               AND d.detail_id = l_exp_dtls_tbl(i).detail_id;
            j := l_exp_dtls_tbl.next(j);
          END LOOP;
        
          -- �޸�״̬  
          assign_change_status(p_expense_id    => p_expenses_tbl(i),
                               x_return_status => l_return_status);
          IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
          END IF;
        END IF;
        ------------------------------------------------------------------------------------------
      END IF;
    
      i := p_expenses_tbl.next(i);
    END LOOP;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END submit;

  PROCEDURE unsubmit(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                     p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                     p_expense_id    IN NUMBER,
                     x_return_status OUT VARCHAR2,
                     x_msg_count     OUT NUMBER,
                     x_msg_data      OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'unsubmit';
  
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_rec     cux_gl_fin_expense_details%ROWTYPE;
    l_limit_amount     NUMBER;
    l_auto_assign_dept VARCHAR2(80);
    l_cnt              NUMBER;
    l_return_status    VARCHAR2(1);
    l_msg_count        NUMBER;
    l_msg_data         VARCHAR2(2000);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    -- ����
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE e.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    IF l_fin_exp_rec.status_code = ('APPROVED') THEN
    
      BEGIN
        SELECT d.*
          INTO l_exp_dtls_rec
          FROM cux_gl_fin_expenses_all e, cux_gl_fin_expense_details_all d
         WHERE d.expense_id = e.expense_id
           AND d.assignment_amount = e.expense_amount
           AND d.status_code NOT IN ('REVERSED', 'CANCELED')
              -- AND d.assignment_dept = '0000000'
              -- AND d.status_code = 'APPROVED'
           AND e.expense_id = p_expense_id;
      EXCEPTION
        WHEN no_data_found THEN
          fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
          fnd_message.set_token('MESSAGE',
                                '���ñ��:' || l_fin_exp_rec.expense_number || ', ��ȷ��, �����в�������г���.');
          fnd_msg_pub.add;
          RAISE fnd_api.g_exc_error;
        WHEN too_many_rows THEN
          fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
          fnd_message.set_token('MESSAGE',
                                '���ñ��:' || l_fin_exp_rec.expense_number || ', ���ڶ��������, ����������ȷ��.');
          fnd_msg_pub.add;
          RAISE fnd_api.g_exc_error;
      END;
    
      -- �޶���Χ��ֱ��ȷ��
      BEGIN
        SELECT attribute1
          INTO l_limit_amount
          FROM cux_gl_fincost_map_all m
         WHERE m.enabled_flag = 'Y'
           AND trunc(SYSDATE) BETWEEN nvl(m.start_date_active, trunc(SYSDATE)) AND
               nvl(m.end_date_active, SYSDATE)
           AND m.org_id = l_fin_exp_rec.org_id
           AND m.expense_type = l_fin_exp_rec.expense_type
           AND m.expense_item = l_fin_exp_rec.expense_item
           AND m.drcr_type = l_fin_exp_rec.drcr_type;
      EXCEPTION
        WHEN OTHERS THEN
          l_limit_amount := NULL;
      END;
    
      -- ֱ��ȷ���ɱ�����
      l_auto_assign_dept := get_ou_fincost_center(p_org_id => l_fin_exp_rec.org_id);
    
      IF l_fin_exp_rec.expense_amount <= nvl(l_limit_amount, 0) AND l_limit_amount IS NOT NULL AND
         l_exp_dtls_rec.assignment_amount = l_fin_exp_rec.expense_amount AND
         l_exp_dtls_rec.assignment_dept = nvl(l_auto_assign_dept, 'X') THEN
        -- ����ȷ��
        cux_gl_fin_expenses_pkg.unconfirm(p_init_msg_list => fnd_api.g_false,
                                          p_commit        => fnd_api.g_false,
                                          p_expense_id    => p_expense_id,
                                          p_detail_id     => l_exp_dtls_rec.detail_id,
                                          x_return_status => l_return_status,
                                          x_msg_count     => l_msg_count,
                                          x_msg_data      => l_msg_data);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      END IF;
    ELSE
      SELECT COUNT(1)
        INTO l_cnt
        FROM cux_gl_fin_expense_details_all d
       WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
         AND d.expense_id = p_expense_id;
    
      -- ���״̬
      IF l_fin_exp_rec.status_code <> 'UNMATCH' OR l_cnt > 0 THEN
        fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_UNSUBMIT');
        fnd_message.set_token('EXPENSE_NUMBER', to_char(l_fin_exp_rec.expense_number));
        fnd_message.set_token('STATUS',
                              get_lookup_meaning(l_fin_exp_rec.status_code, 'CUX_FINCOST_TYPE'));
        fnd_msg_pub.add;
        RAISE fnd_api.g_exc_error;
      END IF;
    
      UPDATE cux_gl_fin_expenses_all e
         SET e.submit_by         = NULL,
             e.submit_date       = NULL,
             e.status_code       = 'NEW',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code = 'UNMATCH'
         AND e.expense_id = p_expense_id;
    END IF;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END unsubmit;

  PROCEDURE unsubmit(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                     p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                     p_expenses_tbl  IN dbms_utility.number_array,
                     x_return_status OUT VARCHAR2,
                     x_msg_count     OUT NUMBER,
                     x_msg_data      OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'unsubmit';
  
    i               NUMBER;
    l_return_status VARCHAR2(1);
    l_msg_count     NUMBER;
    l_msg_data      VARCHAR2(2000);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name || '-1');
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    i := p_expenses_tbl.first;
    WHILE i IS NOT NULL LOOP
      cux_gl_fin_expenses_pkg.unsubmit(p_init_msg_list => fnd_api.g_false,
                                       p_commit        => fnd_api.g_false,
                                       p_expense_id    => p_expenses_tbl(i),
                                       x_return_status => l_return_status,
                                       x_msg_count     => l_msg_count,
                                       x_msg_data      => l_msg_data);
    
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    
      i := p_expenses_tbl.next(i);
    END LOOP;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name || '-1');
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name || '-1');
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name || '-1');
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END unsubmit;

  /*==================================================
  Procedure Name:  insert_details
  Description:
      ��������ύ���
  Argument:
      p_dtls_rec      ����Ҫ����Ĳ��������ϸ��¼
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE insert_details(p_dtls_rec      IN OUT cux_gl_fin_expense_details%ROWTYPE,
                           x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'insert_details';
    l_max_dtls_line NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    SELECT nvl(MAX(detail_line), 0)
      INTO l_max_dtls_line
      FROM cux_gl_fin_expense_details
     WHERE expense_id = p_dtls_rec.expense_id;
  
    p_dtls_rec.detail_line := l_max_dtls_line + 1;
  
    p_dtls_rec.creation_date     := SYSDATE;
    p_dtls_rec.created_by        := fnd_global.user_id;
    p_dtls_rec.last_updated_by   := fnd_global.user_id;
    p_dtls_rec.last_update_date  := SYSDATE;
    p_dtls_rec.last_update_login := fnd_global.login_id;
  
    INSERT INTO cux_gl_fin_expense_details VALUES p_dtls_rec;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END insert_details;

  /*==================================================
  Procedure Name:  split_dtls_line
  Description:
      ��ֲ��������ϸ��
  Argument:
      p_dtls_rec      ����Ҫ����Ĳ��������ϸ��¼
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE split_dtls_line(p_exp_dtls_rec   IN cux_gl_fin_expense_details%ROWTYPE,
                            p_assign_amount1 IN NUMBER,
                            p_assign_amount2 IN NUMBER,
                            x_detail_id      OUT NUMBER,
                            x_return_status  OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'split_dtls_line';
  
    l_max_dtls_line NUMBER;
    l_dtls_new_rec  cux_gl_fin_expense_details%ROWTYPE;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    SELECT nvl(MAX(detail_line), 0)
      INTO l_max_dtls_line
      FROM cux_gl_fin_expense_details
     WHERE expense_id = p_exp_dtls_rec.expense_id;
  
    UPDATE cux_gl_fin_expense_details d
       SET d.assignment_amount = p_assign_amount1,
           d.last_updated_by   = fnd_global.user_id,
           d.last_update_date  = SYSDATE,
           d.last_update_login = fnd_global.login_id
     WHERE d.expense_id = p_exp_dtls_rec.detail_id;
  
    -- �������ϸ��
    l_dtls_new_rec                      := NULL;
    l_dtls_new_rec.detail_id            := cux_gl_fin_expense_details_s.nextval;
    l_dtls_new_rec.expense_id           := p_exp_dtls_rec.expense_id;
    l_dtls_new_rec.org_id               := p_exp_dtls_rec.org_id;
    l_dtls_new_rec.detail_line          := l_max_dtls_line + 1;
    l_dtls_new_rec.assignment_dept      := p_exp_dtls_rec.assignment_dept;
    l_dtls_new_rec.assignment_amount    := p_assign_amount2;
    l_dtls_new_rec.assigned_by          := p_exp_dtls_rec.assigned_by;
    l_dtls_new_rec.confirmed_by         := p_exp_dtls_rec.confirmed_by;
    l_dtls_new_rec.status_code          := p_exp_dtls_rec.status_code;
    l_dtls_new_rec.split_from_detail_id := p_exp_dtls_rec.detail_id;
    l_dtls_new_rec.je_header_id         := p_exp_dtls_rec.je_header_id;
    l_dtls_new_rec.creation_date        := SYSDATE;
    l_dtls_new_rec.created_by           := fnd_global.user_id;
    l_dtls_new_rec.last_updated_by      := fnd_global.user_id;
    l_dtls_new_rec.last_update_date     := SYSDATE;
    l_dtls_new_rec.last_update_login    := fnd_global.login_id;
    l_dtls_new_rec.attribute1           := p_exp_dtls_rec.attribute1;
    l_dtls_new_rec.attribute2           := p_exp_dtls_rec.attribute2;
    l_dtls_new_rec.attribute3           := p_exp_dtls_rec.attribute3;
    l_dtls_new_rec.attribute4           := p_exp_dtls_rec.attribute4;
    l_dtls_new_rec.attribute5           := p_exp_dtls_rec.attribute5;
    l_dtls_new_rec.attribute6           := p_exp_dtls_rec.attribute6;
    l_dtls_new_rec.attribute7           := p_exp_dtls_rec.attribute7;
    l_dtls_new_rec.attribute8           := p_exp_dtls_rec.attribute8;
    l_dtls_new_rec.attribute9           := p_exp_dtls_rec.attribute9;
    l_dtls_new_rec.attribute10          := p_exp_dtls_rec.attribute10;
    l_dtls_new_rec.attribute11          := p_exp_dtls_rec.attribute11;
    l_dtls_new_rec.attribute12          := p_exp_dtls_rec.attribute12;
    l_dtls_new_rec.attribute13          := p_exp_dtls_rec.attribute13;
    l_dtls_new_rec.attribute14          := p_exp_dtls_rec.attribute14;
    l_dtls_new_rec.attribute15          := p_exp_dtls_rec.attribute15;
  
    INSERT INTO cux_gl_fin_expense_details VALUES l_dtls_new_rec;
    x_detail_id := l_dtls_new_rec.detail_id;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END split_dtls_line;

  PROCEDURE assign_change_status(p_expense_id IN NUMBER, x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'assign_change_status';
    l_expense_amount   NUMBER;
    l_assigned_amount  NUMBER;
    l_confirmed_amount NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    SELECT SUM(e.expense_amount)
      INTO l_expense_amount
      FROM cux_gl_fin_expenses_all e
     WHERE e.expense_id = p_expense_id;
  
    SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_assigned_amount
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.assigned_by IS NOT NULL
       AND d.expense_id = p_expense_id;
  
    SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_confirmed_amount
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.confirmed_by IS NOT NULL
       AND d.expense_id = p_expense_id;
  
    IF nvl(l_assigned_amount, 0) = 0 THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'UNMATCH',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('UNMATCH', 'PARTMATCH', 'MATCHED', 'PARTAPPROVE')
         AND e.expense_id = p_expense_id;
    ELSIF nvl(l_expense_amount, 0) > nvl(l_assigned_amount, 0) AND nvl(l_assigned_amount, 0) <> 0 THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'PARTMATCH',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('UNMATCH', 'PARTMATCH', 'MATCHED', 'PARTAPPROVE')
         AND e.expense_id = p_expense_id;
    ELSIF nvl(l_expense_amount, 0) = nvl(l_assigned_amount, 0) AND
          nvl(l_expense_amount, 0) > nvl(l_confirmed_amount, 0) AND nvl(l_confirmed_amount, 0) <> 0 THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'PARTAPPROVE',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('PARTMATCH', 'MATCHED', 'PARTAPPROVE', 'APPROVED')
         AND e.expense_id = p_expense_id;
    ELSIF nvl(l_expense_amount, 0) = nvl(l_assigned_amount, 0) THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'MATCHED',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('UNMATCH', 'PARTMATCH', 'MATCHED', 'PARTAPPROVE')
         AND e.expense_id = p_expense_id;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END assign_change_status;

  /*==================================================
  Procedure Name:  assignment_check
  Description:
      ������÷�������
  Argument:
      p_fin_exp_rec      ���������
      p_assignment_dept  �����䲿��
      p_split_amount     �����η�����
      x_return_status    : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE assignment_check(p_fin_exp_rec     IN cux_gl_fin_expenses%ROWTYPE,
                             p_assignment_dept IN VARCHAR2,
                             p_split_amount    IN NUMBER,
                             p_remarks         IN VARCHAR2,
                             x_return_status   OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'assignment_check';
    l_return_status    VARCHAR2(1);
    l_auto_assign_dept VARCHAR2(80);
    l_limit_amount     NUMBER;
    l_assigned_amount  NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    l_return_status := fnd_api.g_ret_sts_success;
  
    -- ֱ��ȷ�ϳɱ�����
    l_auto_assign_dept := get_ou_fincost_center(p_org_id => p_fin_exp_rec.org_id);
  
    -- �Զ��ύ����
    BEGIN
      SELECT attribute1
        INTO l_limit_amount
        FROM cux_gl_fincost_map_all m
       WHERE m.enabled_flag = 'Y'
         AND trunc(SYSDATE) BETWEEN nvl(m.start_date_active, trunc(SYSDATE)) AND
             nvl(m.end_date_active, SYSDATE)
         AND m.org_id = p_fin_exp_rec.org_id
         AND m.expense_type = p_fin_exp_rec.expense_type
         AND m.expense_item = p_fin_exp_rec.expense_item
         AND m.drcr_type = p_fin_exp_rec.drcr_type;
    EXCEPTION
      WHEN OTHERS THEN
        l_limit_amount := NULL;
    END;
  
    IF p_assignment_dept IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '���䲿��');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF p_split_amount IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '���η�����');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF p_remarks IS NULL AND (nvl(p_assignment_dept, 'X') <> nvl(l_auto_assign_dept, 'X') OR
       (nvl(p_assignment_dept, 'X') = nvl(l_auto_assign_dept, 'X') AND
       p_fin_exp_rec.expense_amount > nvl(l_limit_amount, 0))) THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '��ע');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    -- ���δ������
    SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_assigned_amount
      FROM cux_gl_fin_expense_details d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.assigned_by IS NOT NULL
       AND d.expense_id = p_fin_exp_rec.expense_id;
  
    IF (nvl(p_fin_exp_rec.expense_amount, 0) - nvl(l_assigned_amount, 0)) < nvl(p_split_amount, 0) AND
       nvl(p_split_amount, 0) > 0 THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_SPLIT_AMT_ERROR');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('SPLIT_AMOUNT', to_char(p_split_amount));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    -- ���״̬
    IF p_fin_exp_rec.status_code NOT IN ('PARTMATCH', 'UNMATCH') THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_ASSIGN');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('STATUS',
                            cux_fnd_common_utl.get_lookup_meaning(p_fin_exp_rec.status_code,
                                                                  'CUX_FINCOST_TYPE'));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END assignment_check;

  /*==================================================
  Procedure Name:  assignment_check
  Description:
      ������÷�������
  Argument:
      p_expense_id      ��������ñ�ʶ
      x_return_status    : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE assignment_check(p_fin_exp_rec   IN cux_gl_fin_expenses%ROWTYPE,
                             p_exp_dtls_tbl  IN exp_dtls_tbl_type,
                             x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'assignment_check';
    i                  NUMBER;
    l_return_status    VARCHAR2(1);
    l_auto_assign_dept VARCHAR2(80);
    l_assigned_amount  NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    l_return_status := fnd_api.g_ret_sts_success;
  
    l_auto_assign_dept := get_ou_fincost_center(p_org_id => p_fin_exp_rec.org_id);
  
    -- ���״̬
    IF p_fin_exp_rec.status_code NOT IN ('PARTMATCH', 'UNMATCH') THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_ASSIGN');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('STATUS',
                            cux_fnd_common_utl.get_lookup_meaning(p_fin_exp_rec.status_code,
                                                                  'CUX_FINCOST_TYPE'));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    -- ���δ������
    SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_assigned_amount
      FROM cux_gl_fin_expense_details d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.assigned_by IS NOT NULL
       AND d.expense_id = p_fin_exp_rec.expense_id;
  
    IF (nvl(p_fin_exp_rec.expense_amount, 0) - nvl(l_assigned_amount, 0)) < 0 AND
       nvl(l_assigned_amount, 0) > 0 THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_SPLIT_AMT_ERROR');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('SPLIT_AMOUNT', to_char(l_assigned_amount));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    i := p_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
    
      IF p_exp_dtls_tbl(i).assignment_dept IS NULL THEN
        fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
        fnd_message.set_token('ATTRIBUTE', '���䲿��');
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      IF p_exp_dtls_tbl(i).assignment_amount IS NULL THEN
        fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
        fnd_message.set_token('ATTRIBUTE', '���η�����');
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      IF p_exp_dtls_tbl(i).remarks IS NULL AND nvl(p_exp_dtls_tbl(i).assignment_dept, 'X') <>
          nvl(l_auto_assign_dept, 'X') THEN
        fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
        fnd_message.set_token('ATTRIBUTE', '��ע');
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      i := p_exp_dtls_tbl.next(i);
    END LOOP;
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END assignment_check;

  /*==================================================
  Procedure Name:  assignment
  Description:
      ������÷���
  Argument:
      p_expenses_tbl  ��������õ���
      x_return_status : ���ش������
      x_msg_count     : ������Ϣ����
      x_msg_data      ��������Ϣ�ı�
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE assignment(p_init_msg_list   IN VARCHAR2 DEFAULT fnd_api.g_false,
                       p_commit          IN VARCHAR2 DEFAULT fnd_api.g_true,
                       p_expense_id      IN NUMBER,
                       p_assignment_dept IN VARCHAR2,
                       p_split_amount    IN NUMBER,
                       p_remarks         IN VARCHAR2,
                       x_return_status   OUT VARCHAR2,
                       x_msg_count       OUT NUMBER,
                       x_msg_data        OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'assignment';
  
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_return_status    VARCHAR2(1);
    l_msg_count        NUMBER;
    l_msg_data         VARCHAR2(2000);
    l_dtls_new_rec     cux_gl_fin_expense_details%ROWTYPE;
    l_auto_assign_dept VARCHAR2(80);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    -- ����
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all cgfe
     WHERE cgfe.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    assignment_check(p_fin_exp_rec     => l_fin_exp_rec,
                     p_assignment_dept => p_assignment_dept,
                     p_split_amount    => p_split_amount,
                     p_remarks         => p_remarks,
                     x_return_status   => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    /*SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_assigned_amount
      FROM cux_gl_fin_expense_details d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.assigned_by IS NOT NULL
       AND d.expense_id = l_fin_exp_rec.expense_id;
    
    IF p_assignment_dept = '0000000' THEN
      l_split_amount := l_fin_exp_rec.expense_amount - l_assigned_amount;
    ELSE
      l_split_amount := p_split_amount;
    END IF;*/
  
    -- ����������ϸ��
    l_dtls_new_rec                      := NULL;
    l_dtls_new_rec.detail_id            := cux_gl_fin_expense_details_s.nextval;
    l_dtls_new_rec.expense_id           := l_fin_exp_rec.expense_id;
    l_dtls_new_rec.org_id               := l_fin_exp_rec.org_id;
    l_dtls_new_rec.detail_line          := NULL;
    l_dtls_new_rec.assignment_dept      := p_assignment_dept;
    l_dtls_new_rec.assignment_amount    := p_split_amount;
    l_dtls_new_rec.assigned_by          := fnd_global.user_id;
    l_dtls_new_rec.assigned_date        := SYSDATE;
    l_dtls_new_rec.remarks              := p_remarks;
    l_dtls_new_rec.status_code          := 'MATCHED';
    l_dtls_new_rec.split_from_detail_id := NULL;
  
    insert_details(p_dtls_rec => l_dtls_new_rec, x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- �޸�״̬  
    assign_change_status(p_expense_id => p_expense_id, x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    l_auto_assign_dept := get_ou_fincost_center(p_org_id => l_fin_exp_rec.org_id);
  
    IF p_assignment_dept = nvl(l_auto_assign_dept, 'X') THEN
      -- �Զ�ȷ��
      cux_gl_fin_expenses_pkg.confirm(p_init_msg_list => fnd_api.g_false,
                                      p_commit        => fnd_api.g_false,
                                      p_expense_id    => l_dtls_new_rec.expense_id,
                                      p_detail_id     => l_dtls_new_rec.detail_id,
                                      x_return_status => l_return_status,
                                      x_msg_count     => l_msg_count,
                                      x_msg_data      => l_msg_data);
    
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    END IF;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END assignment;

  PROCEDURE unassign(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                     p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                     p_expense_id    IN NUMBER,
                     p_detail_id     IN NUMBER,
                     x_return_status OUT VARCHAR2,
                     x_msg_count     OUT NUMBER,
                     x_msg_data      OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'unassign';
  
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_rec     cux_gl_fin_expense_details%ROWTYPE;
    l_auto_assign_dept VARCHAR2(80);
    l_limit_amount     NUMBER;
    l_return_status    VARCHAR2(1);
    l_msg_count        NUMBER;
    l_msg_data         VARCHAR2(2000);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    -- ����
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE e.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    SELECT *
      INTO l_exp_dtls_rec
      FROM cux_gl_fin_expense_details_all d
     WHERE d.detail_id = p_detail_id
       FOR UPDATE NOWAIT;
  
    -- ֱ�ӷ����ȷ�ϵĳɱ�����
    l_auto_assign_dept := get_ou_fincost_center(p_org_id => l_fin_exp_rec.org_id);
  
    IF l_exp_dtls_rec.status_code = 'APPROVED' AND
       l_exp_dtls_rec.assignment_dept = nvl(l_auto_assign_dept, 'X') THEN
      -- ����ȷ��
      cux_gl_fin_expenses_pkg.unconfirm(p_init_msg_list => fnd_api.g_false,
                                        p_commit        => fnd_api.g_false,
                                        p_expense_id    => p_expense_id,
                                        p_detail_id     => p_detail_id,
                                        x_return_status => l_return_status,
                                        x_msg_count     => l_msg_count,
                                        x_msg_data      => l_msg_data);
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    
    ELSE
      -- ���״̬
      IF l_fin_exp_rec.status_code NOT IN ('PARTMATCH', 'MATCHED', 'PARTAPPROVE') OR
         l_exp_dtls_rec.status_code NOT IN ('MATCHED') THEN
        fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_UNASSIGN');
        fnd_message.set_token('EXPENSE_NUMBER', to_char(l_fin_exp_rec.expense_number));
        fnd_message.set_token('STATUS',
                              get_lookup_meaning(l_fin_exp_rec.status_code, 'CUX_FINCOST_TYPE'));
        fnd_message.set_token('LINE_STATUS',
                              get_lookup_meaning(l_exp_dtls_rec.status_code,
                                                 'CUX_FINCOST_LINE_TYPE'));
        fnd_msg_pub.add;
        RAISE fnd_api.g_exc_error;
      END IF;
    
      -- ȷ����
      UPDATE cux_gl_fin_expense_details_all d
         SET d.status_code       = 'CANCELED',
             d.last_updated_by   = fnd_global.user_id,
             d.last_update_date  = SYSDATE,
             d.last_update_login = fnd_global.login_id
       WHERE d.status_code = 'MATCHED'
         AND d.detail_id = p_detail_id;
    
      -- �޸�ͷ״̬  
      assign_change_status(p_expense_id => p_expense_id, x_return_status => l_return_status);
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    
      -- �Զ��ύ����
      BEGIN
        SELECT attribute1
          INTO l_limit_amount
          FROM cux_gl_fincost_map_all m
         WHERE m.enabled_flag = 'Y'
           AND trunc(SYSDATE) BETWEEN nvl(m.start_date_active, trunc(SYSDATE)) AND
               nvl(m.end_date_active, SYSDATE)
           AND m.org_id = l_fin_exp_rec.org_id
           AND m.expense_type = l_fin_exp_rec.expense_type
           AND m.expense_item = l_fin_exp_rec.expense_item
           AND m.drcr_type = l_fin_exp_rec.drcr_type;
      EXCEPTION
        WHEN OTHERS THEN
          l_limit_amount := NULL;
      END;
    
      -- �Զ�����&ȷ������
      IF l_fin_exp_rec.expense_amount <= nvl(l_limit_amount, 0) AND l_limit_amount IS NOT NULL AND
         l_exp_dtls_rec.assignment_amount = l_fin_exp_rec.expense_amount AND
         l_exp_dtls_rec.assignment_dept = nvl(l_auto_assign_dept, 'X') THEN
        cux_gl_fin_expenses_pkg.unsubmit(p_init_msg_list => fnd_api.g_false,
                                         p_commit        => fnd_api.g_false,
                                         p_expense_id    => p_expense_id,
                                         x_return_status => l_return_status,
                                         x_msg_count     => l_msg_count,
                                         x_msg_data      => l_msg_data);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      END IF;
    END IF;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END unassign;

  /*==================================================
  Procedure Name:  confirm_change_status
  Description:
      �������ȷ�ϼ��
  Argument:
      p_expense_id    ���������ȷ��״̬�޸�
      
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE confirm_change_status(p_expense_id IN NUMBER, x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'confirm_change_status';
    l_expense_amount   NUMBER;
    l_assigned_amount  NUMBER;
    l_confirmed_amount NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    SELECT SUM(e.expense_amount)
      INTO l_expense_amount
      FROM cux_gl_fin_expenses_all e
     WHERE e.expense_id = p_expense_id;
  
    SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_assigned_amount
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.assigned_by IS NOT NULL
       AND d.expense_id = p_expense_id;
  
    SELECT nvl(SUM(d.assignment_amount), 0)
      INTO l_confirmed_amount
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.confirmed_by IS NOT NULL
       AND d.expense_id = p_expense_id;
  
    IF nvl(l_confirmed_amount, 0) = 0 THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'MATCHED',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('MATCHED', 'PARTAPPROVE', 'APPROVED')
         AND e.expense_id = p_expense_id;
    ELSIF nvl(l_expense_amount, 0) > nvl(l_assigned_amount, 0) AND nvl(l_assigned_amount, 0) <> 0 THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'PARTMATCH',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('MATCHED', 'PARTAPPROVE', 'APPROVED')
         AND e.expense_id = p_expense_id;
    ELSIF nvl(l_expense_amount, 0) > nvl(l_confirmed_amount, 0) AND nvl(l_confirmed_amount, 0) <> 0 THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'PARTAPPROVE',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('MATCHED', 'PARTAPPROVE', 'APPROVED')
         AND e.expense_id = p_expense_id;
    ELSIF nvl(l_expense_amount, 0) = nvl(l_confirmed_amount, 0) THEN
      UPDATE cux_gl_fin_expenses_all e
         SET e.status_code       = 'APPROVED',
             e.last_updated_by   = fnd_global.user_id,
             e.last_update_date  = SYSDATE,
             e.last_update_login = fnd_global.login_id
       WHERE e.status_code IN ('MATCHED', 'PARTAPPROVE')
         AND e.expense_id = p_expense_id;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END confirm_change_status;

  /*==================================================
  Procedure Name:  confirm_check
  Description:
      �������ȷ�ϼ��
  Argument:
      p_expense_id    ��������õ��ݱ�ʶ
      p_detail_id     ��������÷����б�ʶ
      
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE confirm_check(p_fin_exp_rec   IN cux_gl_fin_expenses%ROWTYPE,
                          p_exp_dtls_rec  IN cux_gl_fin_expense_details%ROWTYPE,
                          x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'confirm_check';
    l_return_status VARCHAR2(1);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    l_return_status := fnd_api.g_ret_sts_success;
  
    -- ���״̬
    IF p_fin_exp_rec.status_code NOT IN ('PARTMATCH', 'MATCHED', 'PARTAPPROVE') OR
       p_exp_dtls_rec.status_code NOT IN ('MATCHED') THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_CONFIRM');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('STATUS',
                            cux_fnd_common_utl.get_lookup_meaning(p_fin_exp_rec.status_code,
                                                                  'CUX_FINCOST_TYPE'));
      fnd_message.set_token('LINE_STATUS',
                            cux_fnd_common_utl.get_lookup_meaning(p_exp_dtls_rec.status_code,
                                                                  'CUX_FINCOST_LINE_TYPE'));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END confirm_check;

  /*==================================================
  Procedure Name:  confirm
  Description:
      �������ȷ��
  Argument:
      p_expense_id    ��������õ���
      p_detail_id     : ���÷��÷�����
      x_return_status : ���ش������
      x_msg_count     : ������Ϣ����
      x_msg_data      ��������Ϣ�ı�
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE confirm(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                    p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                    p_expense_id    IN NUMBER,
                    p_detail_id     IN NUMBER,
                    x_return_status OUT VARCHAR2,
                    x_msg_count     OUT NUMBER,
                    x_msg_data      OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'assignment';
  
    l_fin_exp_rec   cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_rec  cux_gl_fin_expense_details%ROWTYPE;
    l_return_status VARCHAR2(1);
  
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    -- ����
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all cgfe
     WHERE cgfe.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    SELECT *
      INTO l_exp_dtls_rec
      FROM cux_gl_fin_expense_details_all cgfe
     WHERE cgfe.detail_id = p_detail_id
       FOR UPDATE NOWAIT;
  
    confirm_check(p_fin_exp_rec   => l_fin_exp_rec,
                  p_exp_dtls_rec  => l_exp_dtls_rec,
                  x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- ȷ����
    UPDATE cux_gl_fin_expense_details_all d
       SET d.status_code       = 'APPROVED',
           d.confirmed_by      = fnd_global.user_id,
           d.confirmed_date    = SYSDATE,
           d.last_updated_by   = fnd_global.user_id,
           d.last_update_date  = SYSDATE,
           d.last_update_login = fnd_global.login_id
     WHERE d.status_code = 'MATCHED'
       AND d.detail_id = p_detail_id;
  
    -- �޸�ͷ״̬  
    confirm_change_status(p_expense_id => p_expense_id, x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END confirm;

  /*==================================================
  Procedure Name:  confirm
  Description:
      �������ȷ��
  Argument:
      p_expense_id    ��������õ���
      p_detail_id     : ���÷��÷�����
      x_return_status : ���ش������
      x_msg_count     : ������Ϣ����
      x_msg_data      ��������Ϣ�ı�
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE unconfirm(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                      p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                      p_expense_id    IN NUMBER,
                      p_detail_id     IN NUMBER,
                      x_return_status OUT VARCHAR2,
                      x_msg_count     OUT NUMBER,
                      x_msg_data      OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'unconfirm';
  
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_rec     cux_gl_fin_expense_details%ROWTYPE;
    l_auto_assign_dept VARCHAR2(80);
    l_return_status    VARCHAR2(1);
    l_msg_count        NUMBER;
    l_msg_data         VARCHAR2(2000);
  
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    -- ����
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE e.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    SELECT *
      INTO l_exp_dtls_rec
      FROM cux_gl_fin_expense_details_all d
     WHERE d.detail_id = p_detail_id
       FOR UPDATE NOWAIT;
  
    -- ֱ�ӷ����ȷ�ϳɱ�����
    l_auto_assign_dept := get_ou_fincost_center(p_org_id => l_fin_exp_rec.org_id);
  
    -- ���״̬
    IF NOT (l_exp_dtls_rec.status_code IN ('APPROVED') AND
        ((l_fin_exp_rec.status_code IN ('PARTMATCH', 'MATCHED') AND
        l_exp_dtls_rec.assignment_dept = nvl(l_auto_assign_dept, 'X')) OR
        l_fin_exp_rec.status_code IN ('APPROVED', 'PARTAPPROVE'))) THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_UNCONFRM');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(l_fin_exp_rec.expense_number));
      fnd_message.set_token('STATUS',
                            get_lookup_meaning(l_fin_exp_rec.status_code, 'CUX_FINCOST_TYPE'));
      fnd_message.set_token('LINE_STATUS',
                            get_lookup_meaning(l_exp_dtls_rec.status_code, 'CUX_FINCOST_LINE_TYPE'));
      fnd_msg_pub.add;
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- ��ȷ����
    UPDATE cux_gl_fin_expense_details_all d
       SET d.status_code       = 'MATCHED',
           d.confirmed_by      = NULL,
           d.last_updated_by   = fnd_global.user_id,
           d.last_update_date  = SYSDATE,
           d.last_update_login = fnd_global.login_id
     WHERE d.status_code = 'APPROVED'
       AND d.detail_id = p_detail_id;
  
    -- �޸�ͷ״̬  
    confirm_change_status(p_expense_id => p_expense_id, x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- �Զ�ȷ�ϵķ�������
    IF l_exp_dtls_rec.assignment_dept = nvl(l_auto_assign_dept, 'X') THEN
      cux_gl_fin_expenses_pkg.unassign(p_init_msg_list => fnd_api.g_false,
                                       p_commit        => fnd_api.g_false,
                                       p_expense_id    => p_expense_id,
                                       p_detail_id     => p_detail_id,
                                       x_return_status => l_return_status,
                                       x_msg_count     => l_msg_count,
                                       x_msg_data      => l_msg_data);
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    END IF;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END unconfirm;

  /*==================================================
  Procedure Name:  create_gl_change_status
  Description:
      �����������GL��״̬�޸�
  Argument:
      p_expense_id    ��������õ��ݱ�ʶ
      
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE create_gl_change_status(p_expense_id    IN NUMBER,
                                    p_group_id      IN NUMBER,
                                    x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'create_gl_change_status';
    l_je_batch_name VARCHAR2(240);
    l_je_header_id  NUMBER;
    l_new_status    VARCHAR2(30);
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    SELECT u.user_name || '|' || e.expense_number
      INTO l_je_batch_name
      FROM cux_gl_fin_expenses_all e, fnd_user u
     WHERE u.user_id = e.created_by
       AND e.expense_id = p_expense_id;
  
    SELECT MAX(jh.je_header_id)
      INTO l_je_header_id
      FROM gl_je_batches jb, gl_je_headers jh
     WHERE jb.je_batch_id = jh.je_batch_id
       AND jb.name LIKE l_je_batch_name || '%'
       AND jb.group_id = p_group_id;
  
    IF l_je_header_id IS NOT NULL THEN
      l_new_status := 'CREATED';
    ELSE
      l_new_status := 'FAILED';
    END IF;
  
    UPDATE cux_gl_fin_expenses_all e
       SET e.status_code       = l_new_status,
           e.last_updated_by   = fnd_global.user_id,
           e.last_update_date  = SYSDATE,
           e.last_update_login = fnd_global.login_id
     WHERE e.status_code = 'CREATING' -- IN ('FAILED', 'APPROVED')
       AND e.expense_id = p_expense_id;
  
    UPDATE cux_gl_fin_expense_details_all d
       SET d.je_header_id      = l_je_header_id,
           d.status_code       = l_new_status,
           d.last_updated_by   = fnd_global.user_id,
           d.last_update_date  = SYSDATE,
           d.last_update_login = fnd_global.login_id
     WHERE d.status_code = 'CREATING' -- IN ('FAILED', 'APPROVED')
       AND d.expense_id = p_expense_id;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END create_gl_change_status;

  /*==================================================
  Procedure Name:  create_gl_check
  Description:
      ������ô����ռ���
  Argument:
      p_expense_id    ��������õ��ݱ�ʶ
      p_detail_id     ��������÷����б�ʶ
      
      x_return_status : ���ش������
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE create_gl_check(p_validation    IN VARCHAR2 DEFAULT 'FORM',
                            p_fin_exp_rec   IN cux_gl_fin_expenses%ROWTYPE,
                            p_exp_dtls_tbl  IN exp_dtls_tbl_type,
                            x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'create_gl_check';
    l_return_status    VARCHAR2(1);
    l_confirmed_amount NUMBER;
    i                  NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    l_return_status := fnd_api.g_ret_sts_success;
  
    IF p_fin_exp_rec.gl_date IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', 'GL����');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF p_fin_exp_rec.bank_department_flag IS NULL THEN
      fnd_message.set_name('ONT', 'OE_ATTRIBUTE_REQUIRED');
      fnd_message.set_token('ATTRIBUTE', '���д���Ŀ�Ƿ�ֲ���');
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    -- ���״̬
    IF (p_validation = 'CREATING' AND p_fin_exp_rec.status_code <> 'CREATING') OR
       (p_validation = 'FORM' AND p_fin_exp_rec.status_code NOT IN ('FAILED', 'APPROVED')) THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_TO_GL');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('STATUS',
                            cux_fnd_common_utl.get_lookup_meaning(p_fin_exp_rec.status_code,
                                                                  'CUX_FINCOST_TYPE'));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    -- �����״̬
    l_confirmed_amount := 0;
    i                  := p_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
      IF ((p_validation = 'CREATING' AND p_exp_dtls_tbl(i).status_code <> 'CREATING') OR
         (p_validation = 'FORM' AND p_exp_dtls_tbl(i).status_code NOT IN ('FAILED', 'APPROVED')) OR p_exp_dtls_tbl(i)
         .je_header_id IS NOT NULL) THEN
        fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_TO_GL');
        fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
        fnd_message.set_token('LINE_NUM', to_char(p_exp_dtls_tbl(i).detail_line));
        fnd_message.set_token('LINE_STATUS',
                              cux_fnd_common_utl.get_lookup_meaning(p_exp_dtls_tbl(i).status_code,
                                                                    'CUX_FINCOST_LINE_TYPE'));
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      IF p_exp_dtls_tbl(i).confirmed_by IS NOT NULL THEN
        l_confirmed_amount := l_confirmed_amount + nvl(p_exp_dtls_tbl(i).assignment_amount, 0);
      END IF;
    
      i := p_exp_dtls_tbl.next(i);
    END LOOP;
  
    -- �����
    IF nvl(p_fin_exp_rec.expense_amount, 0) <> l_confirmed_amount THEN
      fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CONFIRM_AMT_ERR');
      fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
      fnd_message.set_token('CONFIRMED_AMOUNT', to_char(l_confirmed_amount));
      fnd_message.set_token('EXPENSE_AMOUNT', to_char(p_fin_exp_rec.expense_amount));
      fnd_msg_pub.add;
      l_return_status := fnd_api.g_ret_sts_error;
    END IF;
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END create_gl_check;

  PROCEDURE submit_request(p_expense_id IN NUMBER, x_request_id OUT NUMBER) IS
  BEGIN
    -- JXCC:���������������ƾ֤
    x_request_id := fnd_request.submit_request(application => 'CUX',
                                               program     => 'CUX_GL_FIN_EXPENSES_JOURNAL',
                                               description => '',
                                               start_time  => '',
                                               sub_request => FALSE,
                                               argument1   => to_char(p_expense_id));
  END submit_request;

  /*==================================================
  Procedure Name:  confirm
  Description:
      �������ȷ��
  Argument:
      p_expense_id      ��������õ���
      p_gl_date         : ���÷��÷�����
      p_department_flag : �����ʻ���Ŀ�Ƿ����ֲ���
      x_return_status   : ���ش������
      x_msg_count       : ������Ϣ����
      x_msg_data        ��������Ϣ�ı�
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE create_gl(p_init_msg_list   IN VARCHAR2 DEFAULT fnd_api.g_false,
                      p_commit          IN VARCHAR2 DEFAULT fnd_api.g_true,
                      p_expense_id      IN NUMBER,
                      p_gl_date         IN DATE,
                      p_department_flag IN VARCHAR2,
                      x_return_status   OUT VARCHAR2,
                      x_msg_count       OUT NUMBER,
                      x_msg_data        OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'create_gl';
  
    l_fin_exp_rec   cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_tbl  exp_dtls_tbl_type;
    i               NUMBER;
    l_return_status VARCHAR2(1);
    x_request_id    NUMBER;
  
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    -- ����ͷ����
    UPDATE cux_gl_fin_expenses_all e
       SET e.gl_date              = p_gl_date,
           e.bank_department_flag = p_department_flag,
           e.last_updated_by      = fnd_global.user_id,
           e.last_update_date     = SYSDATE,
           e.last_update_login    = fnd_global.login_id
     WHERE e.status_code IN ('FAILED', 'APPROVED')
       AND e.expense_id = p_expense_id;
  
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE e.status_code IN ('FAILED', 'APPROVED')
       AND e.expense_id = p_expense_id;
  
    SELECT *
      BULK COLLECT
      INTO l_exp_dtls_tbl
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
       AND d.confirmed_by IS NOT NULL
       AND d.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    create_gl_check(p_fin_exp_rec   => l_fin_exp_rec,
                    p_exp_dtls_tbl  => l_exp_dtls_tbl,
                    x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- �ύ��������GL
    submit_request(p_expense_id => p_expense_id, x_request_id => x_request_id);
    IF x_request_id > 0 THEN
      NULL;
      -- fnd_message.set_name('FND', 'CONC-REQUEST SUBMITTED');
      -- fnd_message.set_token('JOB', 'JXCC:���������������ƾ֤');
      -- fnd_message.set_token('REQUEST', x_request_id);
      -- fnd_msg_pub.add;
    ELSE
      fnd_message.set_name('FND', 'ECX_CH_ERROR');
      fnd_msg_pub.add;
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- ����ͷ����
    UPDATE cux_gl_fin_expenses_all e
       SET e.status_code       = 'CREATING',
           e.last_updated_by   = fnd_global.user_id,
           e.last_update_date  = SYSDATE,
           e.last_update_login = fnd_global.login_id
     WHERE e.status_code IN ('FAILED', 'APPROVED')
       AND e.expense_id = p_expense_id;
  
    -- ������״̬
    i := l_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
      UPDATE cux_gl_fin_expense_details_all d
         SET d.status_code       = 'CREATING',
             d.last_updated_by   = fnd_global.user_id,
             d.last_update_date  = SYSDATE,
             d.last_update_login = fnd_global.login_id
       WHERE d.status_code NOT IN ('REVERSED', 'CANCELED')
         AND d.confirmed_by IS NOT NULL
         AND d.detail_id = l_exp_dtls_tbl(i).detail_id;
      i := l_exp_dtls_tbl.next(i);
    END LOOP;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END create_gl;

  PROCEDURE log_messages(p_msg_count IN NUMBER, p_msg_data IN VARCHAR2) IS
  BEGIN
    IF p_msg_count = 1 THEN
      log(p_msg_data);
    ELSIF p_msg_count > 1 THEN
      FOR i IN 1 .. p_msg_count LOOP
        log(fnd_msg_pub.get(i, 'F'));
      END LOOP;
    END IF;
  END log_messages;

  PROCEDURE save_api_messages(p_entity_code IN VARCHAR2,
                              p_entity_id   IN NUMBER,
                              p_entity_name IN VARCHAR2) IS
    l_msg_count NUMBER;
    l_msg_data  VARCHAR2(2000);
  BEGIN
    fnd_msg_pub.count_and_get(fnd_api.g_false, l_msg_count, l_msg_data);
  
    IF l_msg_count = 1 THEN
      INSERT INTO cux_gl_fin_expense_msgs
        (transaction_id,
         request_id,
         entity_code,
         entity_id,
         entity_name,
         last_update_login,
         last_updated_by,
         last_update_date,
         created_by,
         creation_date,
         message_type,
         message_text)
      VALUES
        (cux_gl_fin_expense_msgs_s.nextval,
         g_conc_request_id,
         p_entity_code,
         p_entity_id,
         p_entity_name,
         fnd_global.login_id,
         fnd_global.user_id,
         SYSDATE,
         fnd_global.user_id,
         SYSDATE,
         'ERROR',
         l_msg_data);
    ELSIF l_msg_count > 1 THEN
      FOR i IN 1 .. l_msg_count LOOP
        INSERT INTO cux_gl_fin_expense_msgs
          (transaction_id,
           request_id,
           entity_code,
           entity_id,
           entity_name,
           last_update_login,
           last_updated_by,
           last_update_date,
           created_by,
           creation_date,
           message_type,
           message_text)
        VALUES
          (cux_gl_fin_expense_msgs_s.nextval,
           g_conc_request_id,
           p_entity_code,
           p_entity_id,
           p_entity_name,
           fnd_global.login_id,
           fnd_global.user_id,
           SYSDATE,
           fnd_global.user_id,
           SYSDATE,
           'ERROR',
           fnd_msg_pub.get(i, 'F'));
      END LOOP;
    END IF;
  END save_api_messages;

  FUNCTION get_user_name(p_user_id IN NUMBER) RETURN VARCHAR2 IS
    l_user_name fnd_user.user_name%TYPE;
  BEGIN
    SELECT user_name INTO l_user_name FROM fnd_user fu WHERE fu.user_id = p_user_id;
    RETURN l_user_name;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_user_name;

  PROCEDURE get_fincost_map(p_org_id          IN NUMBER,
                            p_drcr_type       IN VARCHAR2,
                            p_expense_type    IN VARCHAR2,
                            p_expense_item    IN VARCHAR2,
                            x_account_segment OUT VARCHAR2,
                            x_cash_flow_item  OUT VARCHAR2) IS
    CURSOR csr_fincost IS
      SELECT m.account_segment, m.cash_flow_item
        FROM cux_gl_fincost_map_all m
       WHERE m.org_id = p_org_id
         AND m.drcr_type = p_drcr_type
         AND m.expense_type = p_expense_type
         AND m.expense_item = p_expense_item
         AND m.enabled_flag = 'Y'
         AND trunc(SYSDATE) BETWEEN nvl(m.start_date_active, trunc(SYSDATE)) AND
             nvl(m.end_date_active, SYSDATE);
  
  BEGIN
    OPEN csr_fincost;
    FETCH csr_fincost
      INTO x_account_segment, x_cash_flow_item;
    CLOSE csr_fincost;
  EXCEPTION
    WHEN OTHERS THEN
      IF csr_fincost%ISOPEN THEN
        CLOSE csr_fincost;
      END IF;
  END get_fincost_map;

  PROCEDURE set_attribute_value(p_descr_flexfield_name    IN VARCHAR2,
                                p_descr_flex_context_code IN VARCHAR2,
                                p_end_user_column_name    IN VARCHAR2,
                                p_attribute_value         IN VARCHAR2,
                                p_x_gl_iface_rec          IN OUT NOCOPY gl_interface%ROWTYPE) IS
    l_attribute_column_name VARCHAR2(30);
  BEGIN
    BEGIN
      SELECT h.application_column_name
        INTO l_attribute_column_name
        FROM apps.fnd_descr_flex_col_usage_vl h, apps.fnd_descriptive_flexs_vl l
       WHERE h.descriptive_flexfield_name = l.descriptive_flexfield_name
         AND h.descriptive_flexfield_name = p_descr_flexfield_name
         AND h.descriptive_flex_context_code = p_descr_flex_context_code
         AND h.end_user_column_name = p_end_user_column_name;
    EXCEPTION
      WHEN no_data_found THEN
        l_attribute_column_name := NULL;
    END;
  
    IF l_attribute_column_name = 'ATTRIBUTE1' THEN
      p_x_gl_iface_rec.attribute1 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE2' THEN
      p_x_gl_iface_rec.attribute2 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE3' THEN
      p_x_gl_iface_rec.attribute3 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE4' THEN
      p_x_gl_iface_rec.attribute4 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE5' THEN
      p_x_gl_iface_rec.attribute5 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE6' THEN
      p_x_gl_iface_rec.attribute6 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE7' THEN
      p_x_gl_iface_rec.attribute7 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE8' THEN
      p_x_gl_iface_rec.attribute8 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE9' THEN
      p_x_gl_iface_rec.attribute9 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE10' THEN
      p_x_gl_iface_rec.attribute10 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE11' THEN
      p_x_gl_iface_rec.attribute11 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE12' THEN
      p_x_gl_iface_rec.attribute12 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE13' THEN
      p_x_gl_iface_rec.attribute13 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE14' THEN
      p_x_gl_iface_rec.attribute14 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE15' THEN
      p_x_gl_iface_rec.attribute15 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE16' THEN
      p_x_gl_iface_rec.attribute16 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE17' THEN
      p_x_gl_iface_rec.attribute17 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE18' THEN
      p_x_gl_iface_rec.attribute18 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE19' THEN
      p_x_gl_iface_rec.attribute19 := p_attribute_value;
    ELSIF l_attribute_column_name = 'ATTRIBUTE20' THEN
      p_x_gl_iface_rec.attribute20 := p_attribute_value;
    END IF;
  END;

  PROCEDURE finexp_to_gl_iface(p_fin_exp_rec     IN cux_gl_fin_expenses%ROWTYPE,
                               p_drcr            IN VARCHAR2,
                               p_period_name     IN VARCHAR2,
                               p_set_of_books_id IN NUMBER,
                               p_ccid            IN NUMBER,
                               p_account_segment IN VARCHAR2,
                               p_cash_flow_item  IN VARCHAR2,
                               p_group_id        IN NUMBER,
                               x_return_status   OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'finexp_to_gl_iface';
  
    l_created_user fnd_user.user_name%TYPE;
    l_gl_iface_rec gl_interface%ROWTYPE;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    -- ������
    l_created_user := get_user_name(p_fin_exp_rec.created_by);
  
    l_gl_iface_rec.status                        := 'NEW';
    g_je_line_num                                := g_je_line_num + 10;
    l_gl_iface_rec.je_line_num                   := g_je_line_num;
    l_gl_iface_rec.set_of_books_id               := p_set_of_books_id;
    l_gl_iface_rec.accounting_date               := p_fin_exp_rec.gl_date;
    l_gl_iface_rec.currency_code                 := p_fin_exp_rec.currency_code;
    l_gl_iface_rec.date_created                  := SYSDATE;
    l_gl_iface_rec.created_by                    := fnd_global.user_id;
    l_gl_iface_rec.actual_flag                   := 'A';
    l_gl_iface_rec.user_je_category_name         := g_user_je_category_name; -- ����ƾ֤
    l_gl_iface_rec.user_je_source_name           := g_user_je_source_name; -- �˹�
    l_gl_iface_rec.code_combination_id           := p_ccid;
    --l_gl_iface_rec.user_currency_conversion_type := p_fin_exp_rec.conversion_type_code;
    --Modifeid By xuliang@20170920
    BEGIN
      SELECT user_conversion_type
        INTO l_gl_iface_rec.user_currency_conversion_type
        FROM gl_daily_conversion_types dct
       WHERE dct.conversion_type = p_fin_exp_rec.conversion_type_code;
    EXCEPTION
      WHEN OTHERS THEN
        l_gl_iface_rec.user_currency_conversion_type := p_fin_exp_rec.conversion_type_code;
    END;
    --Modifeid By xuliang@20170920
    l_gl_iface_rec.currency_conversion_rate      := p_fin_exp_rec.conversion_rate;
    l_gl_iface_rec.currency_conversion_date      := p_fin_exp_rec.conversion_rate_date;
    l_gl_iface_rec.period_name                   := p_period_name;
    l_gl_iface_rec.reference1                    := l_created_user || '|' ||
                                                    p_fin_exp_rec.expense_number; --����
    l_gl_iface_rec.reference2                    := NULL; --������
    l_gl_iface_rec.reference4                    := p_fin_exp_rec.expense_item; --�ռ�����
    l_gl_iface_rec.reference5                    := p_fin_exp_rec.remarks; --�ռ�������
    l_gl_iface_rec.reference6                    := NULL; --�ռ����вο�
    l_gl_iface_rec.reference10                   := p_fin_exp_rec.expense_type || '|' ||
                                                    p_fin_exp_rec.expense_item; --�ռ���������
    l_gl_iface_rec.group_id                      := p_group_id;
  
    IF p_drcr = 'CR' THEN
      l_gl_iface_rec.entered_cr   := p_fin_exp_rec.expense_amount;
      l_gl_iface_rec.accounted_cr := NULL;
    ELSIF p_drcr = 'DR' THEN
      l_gl_iface_rec.entered_dr   := p_fin_exp_rec.expense_amount;
      l_gl_iface_rec.accounted_dr := NULL;
    END IF;
  
    l_gl_iface_rec.context := p_account_segment;
    set_attribute_value(p_descr_flexfield_name    => 'GL_JE_LINES',
                        p_descr_flex_context_code => p_account_segment,
                        p_end_user_column_name    => '�ֽ�����',
                        p_attribute_value         => p_cash_flow_item,
                        p_x_gl_iface_rec          => l_gl_iface_rec);
  
    set_attribute_value(p_descr_flexfield_name    => 'GL_JE_LINES',
                        p_descr_flex_context_code => p_account_segment,
                        p_end_user_column_name    => '�ֽ�����������',
                        p_attribute_value         => '000', -- Ĭ��
                        p_x_gl_iface_rec          => l_gl_iface_rec);
  
    INSERT INTO gl_interface VALUES l_gl_iface_rec;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END finexp_to_gl_iface;

  PROCEDURE expdtls_to_gl_iface(p_fin_exp_rec     IN cux_gl_fin_expenses%ROWTYPE,
                                p_exp_dtls_rec    IN cux_gl_fin_expense_details%ROWTYPE,
                                p_drcr            IN VARCHAR2,
                                p_period_name     IN VARCHAR2,
                                p_set_of_books_id IN NUMBER,
                                p_ccid            IN NUMBER,
                                p_account_segment IN VARCHAR2,
                                p_cash_flow_item  IN VARCHAR2,
                                p_group_id        IN NUMBER,
                                x_return_status   OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'expdtls_to_gl_iface';
  
    l_created_user fnd_user.user_name%TYPE;
    l_gl_iface_rec gl_interface%ROWTYPE;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    -- ������
    l_created_user := get_user_name(p_fin_exp_rec.created_by);
  
    l_gl_iface_rec                               := NULL;
    g_je_line_num                                := g_je_line_num + 10;
    l_gl_iface_rec.je_line_num                   := g_je_line_num;
    l_gl_iface_rec.status                        := 'NEW';
    l_gl_iface_rec.set_of_books_id               := p_set_of_books_id;
    l_gl_iface_rec.accounting_date               := p_fin_exp_rec.gl_date;
    l_gl_iface_rec.currency_code                 := p_fin_exp_rec.currency_code;
    l_gl_iface_rec.date_created                  := SYSDATE;
    l_gl_iface_rec.created_by                    := fnd_global.user_id;
    l_gl_iface_rec.actual_flag                   := 'A';
    l_gl_iface_rec.user_je_category_name         := g_user_je_category_name; -- ����ƾ֤
    l_gl_iface_rec.user_je_source_name           := g_user_je_source_name; -- �˹�
    l_gl_iface_rec.code_combination_id           := p_ccid;
    --l_gl_iface_rec.user_currency_conversion_type := p_fin_exp_rec.conversion_type_code;
    --Modifeid By xuliang@20170920
    BEGIN
      SELECT user_conversion_type
        INTO l_gl_iface_rec.user_currency_conversion_type
        FROM gl_daily_conversion_types dct
       WHERE dct.conversion_type = p_fin_exp_rec.conversion_type_code;
    EXCEPTION
      WHEN OTHERS THEN
        l_gl_iface_rec.user_currency_conversion_type := p_fin_exp_rec.conversion_type_code;
    END;
    --Modifeid By xuliang@20170920
    l_gl_iface_rec.currency_conversion_rate      := p_fin_exp_rec.conversion_rate;
    l_gl_iface_rec.currency_conversion_date      := p_fin_exp_rec.conversion_rate_date;
    l_gl_iface_rec.period_name                   := p_period_name;
    l_gl_iface_rec.reference1                    := l_created_user || '|' ||
                                                    p_fin_exp_rec.expense_number; --����
    l_gl_iface_rec.reference2                    := NULL; --������
    l_gl_iface_rec.reference4                    := p_fin_exp_rec.expense_item; --�ռ�����
    l_gl_iface_rec.reference5                    := p_fin_exp_rec.remarks; --�ռ�������
    l_gl_iface_rec.reference6                    := NULL; --�ռ����вο�
    l_gl_iface_rec.reference10                   := p_fin_exp_rec.expense_type || '|' ||
                                                    p_fin_exp_rec.expense_item || '|' ||
                                                    p_exp_dtls_rec.detail_line; --�ռ���������
    l_gl_iface_rec.group_id                      := p_group_id;
  
    IF p_drcr = 'CR' THEN
      l_gl_iface_rec.entered_cr   := p_exp_dtls_rec.assignment_amount;
      l_gl_iface_rec.accounted_cr := NULL;
    ELSIF p_drcr = 'DR' THEN
      l_gl_iface_rec.entered_dr   := p_exp_dtls_rec.assignment_amount;
      l_gl_iface_rec.accounted_dr := NULL;
    END IF;
  
    l_gl_iface_rec.context := p_account_segment;
    set_attribute_value(p_descr_flexfield_name    => 'GL_JE_LINES',
                        p_descr_flex_context_code => p_account_segment,
                        p_end_user_column_name    => '�ֽ�����',
                        p_attribute_value         => p_cash_flow_item,
                        p_x_gl_iface_rec          => l_gl_iface_rec);
  
    set_attribute_value(p_descr_flexfield_name    => 'GL_JE_LINES',
                        p_descr_flex_context_code => p_account_segment,
                        p_end_user_column_name    => '�ֽ�����������',
                        p_attribute_value         => '000', -- Ĭ��
                        p_x_gl_iface_rec          => l_gl_iface_rec);
  
    INSERT INTO gl_interface VALUES l_gl_iface_rec;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END expdtls_to_gl_iface;

  /*==================================================
  Procedure Name:  journal_import
  Description:
      �������GL�ռ��˵���
  Argument:
      p_expense_id    ��������õ���
  
      x_return_status : ���ش������
      x_msg_count     : ������Ϣ����
      x_msg_data      ��������Ϣ�ı�
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE journal_import(p_expense_id    IN NUMBER,
                           x_return_status OUT VARCHAR2,
                           x_msg_count     OUT NUMBER,
                           x_msg_data      OUT VARCHAR2) IS
    l_api_name VARCHAR2(20) := 'journal_import';
  
    l_fin_exp_rec      cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_tbl     exp_dtls_tbl_type;
    l_set_of_books_id  NUMBER;
    l_coa_id           NUMBER;
    l_interface_run_id NUMBER;
    l_group_id         NUMBER;
    l_bac_segment1     VARCHAR2(25);
    l_bac_segment2     VARCHAR2(25);
    l_bac_segment3     VARCHAR2(25);
    l_bac_segment4     VARCHAR2(25);
    l_bac_segment5     VARCHAR2(25);
    l_bac_segment6     VARCHAR2(25);
    l_bac_segment7     VARCHAR2(25);
    l_dr_segs          VARCHAR2(240);
    l_cr_segs          VARCHAR2(240);
    l_dr_ccid          NUMBER;
    l_cr_ccid          NUMBER;
    i                  NUMBER;
    l_company_segment  VARCHAR2(10);
    x_account_segment  cux_gl_fincost_map.account_segment%TYPE;
    x_cash_flow_item   cux_gl_fincost_map.cash_flow_item%TYPE;
  
    l_period_name   VARCHAR2(15);
    l_return_status VARCHAR2(1);
  
    l_request_id NUMBER;
    l_boolean    BOOLEAN;
    l_phase      VARCHAR2(40);
    l_status     VARCHAR2(40);
    l_dev_phase  VARCHAR2(40);
    l_dev_status VARCHAR2(40);
    l_message    VARCHAR2(255);
    -- x_return_mesg VARCHAR2(30);
    -- x_lockhandle  VARCHAR2(200);
    -- l_lock        NUMBER;
  
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
  
    -- dbms_lock.allocate_unique(lockname   => 'cux_gl_fin_expense' || '-' || to_char(p_expense_id),
    --                           lockhandle => x_lockhandle);
    -- l_lock := dbms_lock.request(lockhandle        => x_lockhandle,
    --                             lockmode          => 6, -- Exclusive��
    --                             timeout           => 5,
    --                             release_on_commit => FALSE);
    -- IF l_lock <> 0 THEN
    --   fnd_message.set_name('FND', 'FND_LOCK_RECORD_ERROR');
    --   fnd_msg_pub.add;
    --   RAISE fnd_api.g_exc_error;
    -- END IF;
  
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE e.status_code = 'CREATING'
       AND e.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    SELECT *
      BULK COLLECT
      INTO l_exp_dtls_tbl
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code = 'CREATING'
       AND d.confirmed_by IS NOT NULL
       AND d.expense_id = p_expense_id
       FOR UPDATE NOWAIT;
  
    -- �����ʷ������Ϣ
    DELETE cux_gl_fin_expense_msgs
     WHERE entity_code = 'FIN_EXPENSE'
       AND entity_id = p_expense_id;
  
    -- �������
    create_gl_check(p_validation    => 'CREATING',
                    p_fin_exp_rec   => l_fin_exp_rec,
                    p_exp_dtls_tbl  => l_exp_dtls_tbl,
                    x_return_status => l_return_status);
    log('����������, l_return_status:' || l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- ��ȡ��Ŀӳ�����Ϣ
    get_fincost_map(p_org_id          => l_fin_exp_rec.org_id,
                    p_drcr_type       => l_fin_exp_rec.drcr_type,
                    p_expense_type    => l_fin_exp_rec.expense_type,
                    p_expense_item    => l_fin_exp_rec.expense_item,
                    x_account_segment => x_account_segment,
                    x_cash_flow_item  => x_cash_flow_item);
    log('��ȡ��Ŀӳ���, x_account_segment:' || x_account_segment || ', x_cash_flow_item:' ||
        x_cash_flow_item);
  
    IF x_account_segment IS NULL THEN
      fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
      fnd_message.set_token('MESSAGE', '���ݷ������ͼ�������Ŀ��ӳ������Ҳ��������Ŀ.');
      fnd_msg_pub.add;
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- ��˾��
    BEGIN
      SELECT hou.short_code, hou.set_of_books_id
        INTO l_company_segment, l_set_of_books_id
        FROM hr_operating_units hou
       WHERE hou.organization_id = l_fin_exp_rec.org_id;
    EXCEPTION
      WHEN OTHERS THEN
        l_company_segment := NULL;
    END;
    log(' l_company_segment:' || l_company_segment || ', l_set_of_books_id:' || l_set_of_books_id);
  
    -- ��ȡ�����˻���Ŀ��
    BEGIN
      SELECT gcc.segment1,
             gcc.segment2,
             gcc.segment3,
             gcc.segment4,
             gcc.segment5,
             gcc.segment6,
             gcc.segment7
        INTO l_bac_segment1,
             l_bac_segment2,
             l_bac_segment3,
             l_bac_segment4,
             l_bac_segment5,
             l_bac_segment6,
             l_bac_segment7
        FROM ce_bank_accounts cba, gl_code_combinations_kfv gcc
       WHERE cba.asset_code_combination_id = gcc.code_combination_id
         AND cba.bank_account_id = l_fin_exp_rec.bank_account_id;
    EXCEPTION
      WHEN no_data_found THEN
        fnd_message.set_name('CE', 'CE_INVALID_BANK');
        fnd_msg_pub.add;
        RAISE fnd_api.g_exc_error;
    END;
    log(' �����˻���Ŀ:' || l_bac_segment1 || '.' || l_bac_segment2 || '.' || l_bac_segment3 || '.' ||
        l_bac_segment4 || '.' || l_bac_segment5 || '.' || l_bac_segment6 || '.' || l_bac_segment7);
  
    -- ת�� ����(�ʽ�)���� �� �ɱ����Ĳ���
    i := l_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
      IF lengthb(l_exp_dtls_tbl(i).assignment_dept) <> 7 THEN
        BEGIN
          SELECT flv.meaning
            INTO l_exp_dtls_tbl(i).assignment_dept
            FROM fnd_lookup_values flv, hr_operating_units hou
           WHERE flv.lookup_type = 'CUX_FINCOST_CENTER'
             AND flv.view_application_id = 20003
             AND flv.language = userenv('LANG')
             AND hou.short_code = flv.tag
             AND hou.organization_id = l_exp_dtls_tbl(i).org_id
             AND flv.attribute1 = l_exp_dtls_tbl(i).assignment_dept;
        EXCEPTION
          WHEN no_data_found THEN
            fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
            fnd_message.set_token('MESSAGE',
                                  '����OU���ҿ��룺CUX_FINCOST_CENTER ���õ�ֱ��ȷ�ϳɱ����Ĳ����ڡ�');
            fnd_msg_pub.add;
            RAISE fnd_api.g_exc_error;
          WHEN OTHERS THEN
            fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
            fnd_message.set_token('MESSAGE',
                                  '����OU���ҿ��룺CUX_FINCOST_CENTER ���õ�ֱ��ȷ�ϳɱ�����ʱ����������' || SQLERRM || '��');
            fnd_msg_pub.add;
            RAISE fnd_api.g_exc_error;
        END;
      END IF;
      i := l_exp_dtls_tbl.next(i);
    END LOOP;
  
    -----------------------------------------------------------------------------------------------
    -- ���� GL_INTERFACE
  
    --��������ID
    l_interface_run_id := gl_interface_control_pkg.get_unique_run_id;
    l_group_id         := gl_interface_control_pkg.get_unique_id;
    l_coa_id           := gl_ci_remote_invoke_pkg.get_coa_id(l_set_of_books_id);
  
    log(' l_interface_run_id:' || l_interface_run_id || ', l_group_id:' || l_group_id ||
        ', l_coa_id:' || l_coa_id);
  
    -- �к�
    g_je_line_num := 0;
  
    --���ɽӿڿ���
    gl_journal_import_pkg.populate_interface_control(g_user_je_source_name,
                                                     l_group_id,
                                                     l_set_of_books_id,
                                                     l_interface_run_id,
                                                     'GL_INTERFACE',
                                                     NULL);
    log(' ���ɽӿڿ��Ʊ�������ɣ�');
  
    IF l_fin_exp_rec.bank_department_flag = 'Y' THEN
      -- �ֲ���ʱ
      i := l_exp_dtls_tbl.first;
      WHILE i IS NOT NULL LOOP
      
        IF l_fin_exp_rec.drcr_type = 'DR' THEN
          -- ��: �跽
          l_dr_segs := l_bac_segment1 || '.' || l_exp_dtls_tbl(i).assignment_dept || '.' ||
                       l_bac_segment3 || '.' || l_bac_segment4 || '.' || l_bac_segment5 || '.' ||
                       l_bac_segment6 || '.' || l_bac_segment7;
        
          l_dr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                             key_flex_code          => 'GL#',
                                             structure_number       => l_coa_id,
                                             validation_date        => l_fin_exp_rec.gl_date,
                                             concatenated_segments  => l_dr_segs);
          log(' ��:' || l_exp_dtls_tbl(i).detail_line || ', l_dr_segs:' || l_dr_segs ||
              ', l_dr_ccid:' || l_dr_ccid);
        
          -- ��: ����                         
          l_cr_segs := l_company_segment || '.' || l_exp_dtls_tbl(i).assignment_dept || '.' ||
                       x_account_segment || '.' || '000000' || '.' || '000' || '.' || '00000000000' || '.' ||
                       '000000';
        
          l_cr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                             key_flex_code          => 'GL#',
                                             structure_number       => l_coa_id,
                                             validation_date        => l_fin_exp_rec.gl_date,
                                             concatenated_segments  => l_cr_segs);
          log(' ��:' || l_exp_dtls_tbl(i).detail_line || ', l_cr_segs:' || l_cr_segs ||
              ', l_cr_ccid:' || l_cr_ccid);
        ELSIF l_fin_exp_rec.drcr_type = 'CR' THEN
          -- ֧: �跽
          l_dr_segs := l_company_segment || '.' || l_exp_dtls_tbl(i).assignment_dept || '.' ||
                       x_account_segment || '.' || '000000' || '.' || '000' || '.' || '00000000000' || '.' ||
                       '000000';
        
          l_dr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                             key_flex_code          => 'GL#',
                                             structure_number       => l_coa_id,
                                             validation_date        => l_fin_exp_rec.gl_date,
                                             concatenated_segments  => l_dr_segs);
          log(' ��:' || l_exp_dtls_tbl(i).detail_line || ', l_dr_segs:' || l_dr_segs ||
              ', l_dr_ccid:' || l_dr_ccid);
        
          -- ֧: ����
          l_cr_segs := l_bac_segment1 || '.' || l_exp_dtls_tbl(i).assignment_dept || '.' ||
                       l_bac_segment3 || '.' || l_bac_segment4 || '.' || l_bac_segment5 || '.' ||
                       l_bac_segment6 || '.' || l_bac_segment7;
        
          l_cr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                             key_flex_code          => 'GL#',
                                             structure_number       => l_coa_id,
                                             validation_date        => l_fin_exp_rec.gl_date,
                                             concatenated_segments  => l_cr_segs);
          log(' ��:' || l_exp_dtls_tbl(i).detail_line || ', l_cr_segs:' || l_cr_segs ||
              ', l_cr_ccid:' || l_cr_ccid);
        
        END IF;
      
        expdtls_to_gl_iface(p_fin_exp_rec     => l_fin_exp_rec,
                            p_exp_dtls_rec    => l_exp_dtls_tbl(i),
                            p_drcr            => 'DR',
                            p_period_name     => l_period_name,
                            p_set_of_books_id => l_set_of_books_id,
                            p_ccid            => l_dr_ccid,
                            p_account_segment => regexp_substr(l_dr_segs, '[^.]+', 1, 3),
                            p_cash_flow_item  => x_cash_flow_item,
                            p_group_id        => l_group_id,
                            x_return_status   => l_return_status);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      
        expdtls_to_gl_iface(p_fin_exp_rec     => l_fin_exp_rec,
                            p_exp_dtls_rec    => l_exp_dtls_tbl(i),
                            p_drcr            => 'CR',
                            p_period_name     => l_period_name,
                            p_set_of_books_id => l_set_of_books_id,
                            p_ccid            => l_cr_ccid,
                            p_account_segment => regexp_substr(l_cr_segs, '[^.]+', 1, 3),
                            p_cash_flow_item  => x_cash_flow_item,
                            p_group_id        => l_group_id,
                            x_return_status   => l_return_status);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      
        i := l_exp_dtls_tbl.next(i);
      END LOOP;
    ELSE
      -- ���ֲ���ʱ
      IF l_fin_exp_rec.drcr_type = 'DR' THEN
        -- ��: �跽
        l_dr_segs := l_bac_segment1 || '.' || '0000000' || '.' || l_bac_segment3 || '.' ||
                     l_bac_segment4 || '.' || l_bac_segment5 || '.' || l_bac_segment6 || '.' ||
                     l_bac_segment7;
      
        l_dr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                           key_flex_code          => 'GL#',
                                           structure_number       => l_coa_id,
                                           validation_date        => l_fin_exp_rec.gl_date,
                                           concatenated_segments  => l_dr_segs);
        log(' l_dr_segs:' || l_dr_segs || ', l_dr_ccid:' || l_dr_ccid);
      
        finexp_to_gl_iface(p_fin_exp_rec     => l_fin_exp_rec,
                           p_drcr            => 'DR',
                           p_period_name     => l_period_name,
                           p_set_of_books_id => l_set_of_books_id,
                           p_ccid            => l_dr_ccid,
                           p_account_segment => regexp_substr(l_dr_segs, '[^.]+', 1, 3),
                           p_cash_flow_item  => x_cash_flow_item,
                           p_group_id        => l_group_id,
                           x_return_status   => l_return_status);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      
        -- ��: ���� 
        i := l_exp_dtls_tbl.first;
        WHILE i IS NOT NULL LOOP
          l_cr_segs := l_company_segment || '.' || l_exp_dtls_tbl(i).assignment_dept || '.' ||
                       x_account_segment || '.' || '000000' || '.' || '000' || '.' || '00000000000' || '.' ||
                       '000000';
        
          l_cr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                             key_flex_code          => 'GL#',
                                             structure_number       => l_coa_id,
                                             validation_date        => l_fin_exp_rec.gl_date,
                                             concatenated_segments  => l_cr_segs);
          log(' ��:' || l_exp_dtls_tbl(i).detail_line || ', l_cr_segs:' || l_cr_segs ||
              ', l_cr_ccid:' || l_cr_ccid);
        
          expdtls_to_gl_iface(p_fin_exp_rec     => l_fin_exp_rec,
                              p_exp_dtls_rec    => l_exp_dtls_tbl(i),
                              p_drcr            => 'CR',
                              p_period_name     => l_period_name,
                              p_set_of_books_id => l_set_of_books_id,
                              p_ccid            => l_cr_ccid,
                              p_account_segment => regexp_substr(l_cr_segs, '[^.]+', 1, 3),
                              p_cash_flow_item  => x_cash_flow_item,
                              p_group_id        => l_group_id,
                              x_return_status   => l_return_status);
          IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
          END IF;
        
          i := l_exp_dtls_tbl.next(i);
        END LOOP;
      ELSIF l_fin_exp_rec.drcr_type = 'CR' THEN
        -- ֧: �跽
        i := l_exp_dtls_tbl.first;
        WHILE i IS NOT NULL LOOP
          l_dr_segs := l_company_segment || '.' || l_exp_dtls_tbl(i).assignment_dept || '.' ||
                       x_account_segment || '.' || '000000' || '.' || '000' || '.' || '00000000000' || '.' ||
                       '000000';
        
          l_dr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                             key_flex_code          => 'GL#',
                                             structure_number       => l_coa_id,
                                             validation_date        => l_fin_exp_rec.gl_date,
                                             concatenated_segments  => l_dr_segs);
          log(' ��:' || l_exp_dtls_tbl(i).detail_line || ', l_dr_segs:' || l_dr_segs ||
              ', l_dr_ccid:' || l_dr_ccid);
        
          expdtls_to_gl_iface(p_fin_exp_rec     => l_fin_exp_rec,
                              p_exp_dtls_rec    => l_exp_dtls_tbl(i),
                              p_drcr            => 'DR',
                              p_period_name     => l_period_name,
                              p_set_of_books_id => l_set_of_books_id,
                              p_ccid            => l_dr_ccid,
                              p_account_segment => regexp_substr(l_dr_segs, '[^.]+', 1, 3),
                              p_cash_flow_item  => x_cash_flow_item,
                              p_group_id        => l_group_id,
                              x_return_status   => l_return_status);
          IF l_return_status <> fnd_api.g_ret_sts_success THEN
            RAISE fnd_api.g_exc_error;
          END IF;
        
          i := l_exp_dtls_tbl.next(i);
        END LOOP;
      
        -- ֧: ����
        l_cr_segs := l_bac_segment1 || '.' || '0000000' || '.' || l_bac_segment3 || '.' ||
                     l_bac_segment4 || '.' || l_bac_segment5 || '.' || l_bac_segment6 || '.' ||
                     l_bac_segment7;
      
        l_cr_ccid := fnd_flex_ext.get_ccid(application_short_name => 'SQLGL',
                                           key_flex_code          => 'GL#',
                                           structure_number       => l_coa_id,
                                           validation_date        => l_fin_exp_rec.gl_date,
                                           concatenated_segments  => l_cr_segs);
        log(' l_cr_segs:' || l_cr_segs || ', l_cr_ccid:' || l_cr_ccid);
      
        finexp_to_gl_iface(p_fin_exp_rec     => l_fin_exp_rec,
                           p_drcr            => 'CR',
                           p_period_name     => l_period_name,
                           p_set_of_books_id => l_set_of_books_id,
                           p_ccid            => l_cr_ccid,
                           p_account_segment => regexp_substr(l_cr_segs, '[^.]+', 1, 3),
                           p_cash_flow_item  => x_cash_flow_item,
                           p_group_id        => l_group_id,
                           x_return_status   => l_return_status);
        IF l_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE fnd_api.g_exc_error;
        END IF;
      END IF;
    END IF;
    -----------------------------------------------------------------------------------------------  
    l_request_id := fnd_request.submit_request(application => 'SQLGL',
                                               program     => 'GLLEZL',
                                               description => '',
                                               start_time  => '',
                                               sub_request => FALSE,
                                               argument1   => to_char(l_interface_run_id),
                                               argument2   => to_char(l_set_of_books_id),
                                               argument3   => 'N',
                                               argument4   => NULL,
                                               argument5   => NULL,
                                               argument6   => 'N',
                                               argument7   => 'O');
    log(' ���ύ"�ռ��˵���"����:' || l_request_id);
    COMMIT;
  
    IF l_request_id > 0 THEN
      NULL;
      -- LEZS0000 �ѳɹ������ռ��˵��벢������ &REQ_ID.
      -- fnd_message.set_name('SQLGL', 'LEZS0000');
      -- fnd_message.set_token('REQ_ID', l_request_id);
      -- fnd_msg_pub.add;
    ELSE
      -- LEZS0001 δ���ύ�ռ��˵�������
      fnd_message.set_name('SQLGL', 'LEZS0001');
      fnd_msg_pub.add;
      RAISE fnd_api.g_exc_error;
    END IF;
  
    l_boolean := fnd_concurrent.wait_for_request(l_request_id,
                                                 10,
                                                 0,
                                                 l_phase,
                                                 l_status,
                                                 l_dev_phase,
                                                 l_dev_status,
                                                 l_message);
    log(' "�ռ��˵���"����:' || l_request_id || ', ������ɽ��:' || l_phase || ', ' || l_status || ',' ||
        l_message);
    IF NOT (l_dev_phase = 'COMPLETE' AND l_dev_status = 'NORMAL') OR l_boolean = FALSE THEN
      -- �ռ��˵���ʧ�ܡ�����ִ�б������ҳ�ԭ��
      -- fnd_message.set_name('SQLGL', 'GL_US_CI_JOURNAL_IMPORT_FAIL');
      fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
      fnd_message.set_token('MESSAGE', '����������������' || l_request_id || '�����');
      fnd_msg_pub.add;
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- �޸�״̬  
    create_gl_change_status(p_expense_id    => p_expense_id,
                            p_group_id      => l_group_id,
                            x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      -- l_lock := dbms_lock.release(x_lockhandle);
      create_gl_change_status(p_expense_id    => p_expense_id,
                              p_group_id      => l_group_id,
                              x_return_status => l_return_status);
    
      DELETE gl_interface
       WHERE user_je_source_name = g_user_je_source_name
         AND group_id = l_group_id;
      DELETE gl_interface_control
       WHERE je_source_name = g_user_je_source_name
         AND group_id = l_group_id;
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      -- l_lock := dbms_lock.release(x_lockhandle);
      create_gl_change_status(p_expense_id    => p_expense_id,
                              p_group_id      => l_group_id,
                              x_return_status => l_return_status);
    
      DELETE gl_interface
       WHERE user_je_source_name = g_user_je_source_name
         AND group_id = l_group_id;
      DELETE gl_interface_control
       WHERE je_source_name = g_user_je_source_name
         AND group_id = l_group_id;
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    WHEN OTHERS THEN
      -- l_lock := dbms_lock.release(x_lockhandle);
      create_gl_change_status(p_expense_id    => p_expense_id,
                              p_group_id      => l_group_id,
                              x_return_status => l_return_status);
    
      DELETE gl_interface
       WHERE user_je_source_name = g_user_je_source_name
         AND group_id = l_group_id;
      DELETE gl_interface_control
       WHERE je_source_name = g_user_je_source_name
         AND group_id = l_group_id;
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
  END;

  /*==================================================
  Procedure Name:  conc_journal
  Description:
      ����������ã�JXCC:���������������ƾ֤
  Argument:
      p_expense_id    ��������õ���
  
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE conc_journal(errbuf       OUT NOCOPY VARCHAR2,
                         retcode      OUT NOCOPY VARCHAR2,
                         p_expense_id IN NUMBER) IS
    l_api_name VARCHAR2(20) := 'conc_journal';
  
    x_return_status VARCHAR2(1);
    x_msg_count     NUMBER;
    x_msg_data      VARCHAR2(2000);
  BEGIN
    log('p_expense_id:' || p_expense_id);
  
    journal_import(p_expense_id, x_return_status, x_msg_count, x_msg_data);
  
    log_messages(p_msg_count => x_msg_count, p_msg_data => x_msg_data);
    save_api_messages(p_entity_code => 'FIN_EXPENSE',
                      p_entity_id   => p_expense_id,
                      p_entity_name => NULL);
    IF x_return_status = fnd_api.g_ret_sts_error THEN
      RAISE fnd_api.g_exc_error;
    ELSIF x_return_status = fnd_api.g_ret_sts_unexp_error THEN
      RAISE fnd_api.g_exc_unexpected_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      retcode := '1';
    WHEN fnd_api.g_exc_unexpected_error THEN
      retcode := '2';
    WHEN OTHERS THEN
      retcode := '2';
      log('����� ' || g_pkg_name || ' ���� ' || l_api_name || ' �г��ִ��� ' || SQLERRM);
      errbuf := SQLERRM;
  END conc_journal;

  FUNCTION get_messages(p_msg_count IN NUMBER, p_msg_data IN VARCHAR2) RETURN VARCHAR2 IS
    l_msg_data VARCHAR2(2000);
  BEGIN
    IF p_msg_count = 1 THEN
      RETURN(p_msg_data);
    ELSIF p_msg_count > 1 THEN
      FOR i IN 1 .. p_msg_count LOOP
        l_msg_data := substrb(l_msg_data || ',' || fnd_msg_pub.get(i, 'F'), 1, 2000);
      END LOOP;
      l_msg_data := ltrim(l_msg_data, ',');
      RETURN l_msg_data;
    END IF;
  
    RETURN l_msg_data;
  END get_messages;

  PROCEDURE check_reversal(p_fin_exp_rec   IN cux_gl_fin_expenses%ROWTYPE,
                           p_exp_dtls_tbl  IN exp_dtls_tbl_type,
                           x_return_status OUT VARCHAR2) IS
    l_api_name CONSTANT VARCHAR2(30) := 'check_reversal';
  
    l_accrual_rev_status VARCHAR2(1);
    l_return_status      VARCHAR2(1);
    i                    NUMBER;
  BEGIN
    x_return_status := fnd_api.g_ret_sts_success;
    l_return_status := fnd_api.g_ret_sts_success;
  
    i := p_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
      IF p_fin_exp_rec.status_code <> 'CREATED' OR p_exp_dtls_tbl(i).status_code <> 'CREATED' THEN
        fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_CANNOT_CONFIRM');
        fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
        fnd_message.set_token('STATUS',
                              cux_fnd_common_utl.get_lookup_meaning(p_fin_exp_rec.status_code,
                                                                    'CUX_FINCOST_TYPE'));
        fnd_message.set_token('LINE_STATUS',
                              cux_fnd_common_utl.get_lookup_meaning(p_exp_dtls_tbl(i).status_code,
                                                                    'CUX_FINCOST_LINE_TYPE'));
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      -- δ����
      SELECT accrual_rev_status
        INTO l_accrual_rev_status
        FROM gl_je_headers jh
       WHERE jh.je_header_id = p_exp_dtls_tbl(i).je_header_id;
      IF nvl(l_accrual_rev_status, 'N') <> 'R' THEN
        fnd_message.set_name('CUX', 'CUX_GL_FIN_EXP_LINE_GL_UNREV');
        fnd_message.set_token('EXPENSE_NUMBER', to_char(p_fin_exp_rec.expense_number));
        fnd_message.set_token('LINE_NUM', to_char(p_exp_dtls_tbl(i).detail_line));
        fnd_msg_pub.add;
        l_return_status := fnd_api.g_ret_sts_error;
      END IF;
    
      i := p_exp_dtls_tbl.next(i);
    END LOOP;
  
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
  END check_reversal;

  PROCEDURE reversal_to_confim(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                               p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                               p_expense_id    IN NUMBER,
                               x_return_status OUT VARCHAR2,
                               x_msg_count     OUT NUMBER,
                               x_msg_data      OUT VARCHAR2) IS
    l_api_name VARCHAR2(20) := 'reversal_to_confim';
  
    i               NUMBER;
    l_fin_exp_rec   cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_tbl  exp_dtls_tbl_type;
    l_return_status VARCHAR2(1);
    x_detail_id     NUMBER;
  BEGIN
    log('���� ' || g_pkg_name || '.' || l_api_name || ', ��ʼ����...');
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE 1 = 1
       AND e.expense_id = p_expense_id;
  
    SELECT *
      BULK COLLECT
      INTO l_exp_dtls_tbl
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('CANCELED', 'REVERSED')
       AND d.expense_id = p_expense_id;
  
    -- ���
    check_reversal(p_fin_exp_rec   => l_fin_exp_rec,
                   p_exp_dtls_tbl  => l_exp_dtls_tbl,
                   x_return_status => l_return_status);
    IF l_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE fnd_api.g_exc_error;
    END IF;
  
    -- �����
    i := l_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
    
      split_dtls_line(p_exp_dtls_rec   => l_exp_dtls_tbl(i),
                      p_assign_amount1 => l_exp_dtls_tbl(i).assignment_amount,
                      p_assign_amount2 => l_exp_dtls_tbl(i).assignment_amount,
                      x_detail_id      => x_detail_id,
                      x_return_status  => l_return_status);
      IF l_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_error;
      END IF;
    
      -- ���¾���״̬
      UPDATE cux_gl_fin_expense_details_all
         SET status_code = 'REVERSED' -- �ѳ���
       WHERE status_code = 'CREATED'
         AND detail_id = l_exp_dtls_tbl(i).detail_id;
    
      -- ��������״̬
      UPDATE cux_gl_fin_expense_details_all
         SET status_code  = 'APPROVED', -- ��ȷ��
             je_header_id = NULL
       WHERE status_code = 'CREATED'
         AND detail_id = x_detail_id;
    
      i := l_exp_dtls_tbl.next(i);
    END LOOP;
  
    -- ����״̬
    UPDATE cux_gl_fin_expenses_all
       SET status_code = 'APPROVED' -- ��ȷ��
     WHERE status_code = 'CREATED'
       AND expense_id = p_expense_id;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    log('���� ' || g_pkg_name || '.' || l_api_name || ', ������ɡ�');
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
      log('���� ' || g_pkg_name || '.' || l_api_name || ', ����������ֹ��');
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
      log('���� ' || g_pkg_name || '.' || l_api_name || ', �����쳣��ֹ��');
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
      log('���� ' || g_pkg_name || '.' || l_api_name || ', ����δ֪�쳣��ֹ��');
  END reversal_to_confim;

  PROCEDURE failed_to_confim(p_init_msg_list IN VARCHAR2 DEFAULT fnd_api.g_false,
                             p_commit        IN VARCHAR2 DEFAULT fnd_api.g_true,
                             p_expense_id    IN NUMBER,
                             x_return_status OUT VARCHAR2,
                             x_msg_count     OUT NUMBER,
                             x_msg_data      OUT VARCHAR2) IS
    l_api_name VARCHAR2(20) := 'failed_to_confim';
  
    i               NUMBER;
    l_fin_exp_rec   cux_gl_fin_expenses%ROWTYPE;
    l_exp_dtls_tbl  exp_dtls_tbl_type;
    l_return_status VARCHAR2(1);
    x_detail_id     NUMBER;
  BEGIN
    log('���� ' || g_pkg_name || '.' || l_api_name || ', ��ʼ����...');
    x_return_status := fnd_api.g_ret_sts_success;
    dbms_transaction.savepoint(l_api_name);
  
    IF fnd_api.to_boolean(p_init_msg_list) THEN
      fnd_msg_pub.initialize;
    END IF;
  
    SELECT *
      INTO l_fin_exp_rec
      FROM cux_gl_fin_expenses_all e
     WHERE 1 = 1
       AND e.expense_id = p_expense_id;
  
    SELECT *
      BULK COLLECT
      INTO l_exp_dtls_tbl
      FROM cux_gl_fin_expense_details_all d
     WHERE d.status_code NOT IN ('CANCELED', 'REVERSED')
          -- AND d.status_code = 'FAILED'
       AND d.expense_id = p_expense_id;
  
    -- ���
    IF l_fin_exp_rec.status_code <> 'FAILED' THEN
      fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
      fnd_message.set_token('MESSAGE',
                            '���ñ��:' || l_fin_exp_rec.expense_number || ', ״̬:' ||
                            cux_gl_fin_expenses_pkg.get_lookup_meaning(l_fin_exp_rec.status_code,
                                                                       'CUX_FINCOST_TYPE') ||
                            ' ��������³���ȷ��״̬.');
      fnd_msg_pub.add;
      RAISE fnd_api.g_exc_error;
    END IF;
  
    i := l_exp_dtls_tbl.first;
    WHILE i IS NOT NULL LOOP
    
      IF l_exp_dtls_tbl(i).status_code <> 'FAILED' THEN
        fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
        fnd_message.set_token('MESSAGE',
                              '���ñ��:' || l_fin_exp_rec.expense_number || ', ��״̬:' ||
                              cux_gl_fin_expenses_pkg.get_lookup_meaning(l_exp_dtls_tbl(i)
                                                                         .status_code,
                                                                         'CUX_FINCOST_LINE_TYPE') ||
                              ' ��������³���ȷ��״̬.');
        fnd_msg_pub.add;
        RAISE fnd_api.g_exc_error;
      END IF;
    
      -- ������״̬
      UPDATE cux_gl_fin_expense_details_all
         SET status_code = 'APPROVED' -- �ѳ���
       WHERE status_code = 'FAILED'
         AND detail_id = l_exp_dtls_tbl(i).detail_id;
    
      i := l_exp_dtls_tbl.next(i);
    END LOOP;
  
    -- ����״̬
    UPDATE cux_gl_fin_expenses_all
       SET status_code = 'APPROVED' -- ��ȷ��
     WHERE status_code = 'FAILED'
       AND expense_id = p_expense_id;
  
    IF fnd_api.to_boolean(p_commit) THEN
      COMMIT;
    END IF;
  
    fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
    log('���� ' || g_pkg_name || '.' || l_api_name || ', ������ɡ�');
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
      log('���� ' || g_pkg_name || '.' || l_api_name || ', ����������ֹ��');
    WHEN fnd_api.g_exc_unexpected_error THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
      log('���� ' || g_pkg_name || '.' || l_api_name || ', �����쳣��ֹ��');
    WHEN OTHERS THEN
      dbms_transaction.rollback_savepoint(l_api_name);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      fnd_msg_pub.add_exc_msg(g_pkg_name, l_api_name);
      fnd_msg_pub.count_and_get(fnd_api.g_false, x_msg_count, x_msg_data);
      log('���� ' || g_pkg_name || '.' || l_api_name || ', ����δ֪�쳣��ֹ��');
  END failed_to_confim;

  /*==================================================
  Procedure Name:  conc_reversal
  Description:
      ����������ã�JXCC:�������ƾ֤��������
  Argument:
      p_org_id    ��������õ���
  
  History:
           1.00  2017-07-21  Liuby  Creation
  ==================================================*/
  PROCEDURE conc_reversal(errbuf   OUT NOCOPY VARCHAR2,
                          retcode  OUT NOCOPY VARCHAR2,
                          p_org_id IN NUMBER DEFAULT NULL) IS
    l_api_name VARCHAR2(20) := 'conc_reversal';
  
    CURSOR csr_expenses IS
      SELECT e.expense_id, e.expense_number
        FROM cux_gl_fin_expenses_all e
       WHERE e.status_code = 'CREATED'
         AND EXISTS (SELECT 1
                FROM cux_gl_fin_expense_details_all d, gl_je_headers jh
               WHERE d.status_code NOT IN ('CANCELED', 'REVERSED')
                 AND d.je_header_id IS NOT NULL
                 AND d.je_header_id = jh.je_header_id
                 AND jh.accrual_rev_status = 'R'
                 AND d.expense_id = e.expense_id)
         AND (e.org_id = p_org_id OR p_org_id IS NULL);
  
    l_status_desc   VARCHAR2(360);
    x_return_status VARCHAR2(1);
    x_msg_count     NUMBER;
    x_msg_data      VARCHAR2(2000);
  BEGIN
    retcode := 0;
    log('p_org_id:' || p_org_id);
  
    output('<html>');
    output('<head>');
    output('<meta http-equiv="Content-Type" content="text/html; charset=utf-8">');
    output('<title>�ޱ����ĵ�</title>');
    output('</head>');
    output('<body>');
  
    output('<table width="100%" border="1">');
    output('<tr><th>���ñ��</th>');
    output('<th>����״̬</th>');
    output('<th>������Ϣ</th></tr>');
  
    FOR i IN csr_expenses LOOP
      -- ����ƾ֤����
      log('����������õ���:' || to_char(i.expense_number));
      reversal_to_confim(p_init_msg_list => fnd_api.g_true,
                         p_commit        => fnd_api.g_true,
                         p_expense_id    => i.expense_id,
                         x_return_status => x_return_status,
                         x_msg_count     => x_msg_count,
                         x_msg_data      => x_msg_data);
    
      IF x_return_status = fnd_api.g_ret_sts_success THEN
        l_status_desc := '�ɹ�';
      ELSIF x_return_status = fnd_api.g_ret_sts_error THEN
        l_status_desc := '����';
        retcode       := greatest(retcode, 1);
      ELSE
        l_status_desc := 'δ֪����';
        retcode       := greatest(retcode, 2);
      END IF;
      output('<tr><td>' || to_char(i.expense_number) || '</td>');
      output('<td>' || l_status_desc || '</td>');
      output('<td>' || get_messages(x_msg_count, x_msg_data) || '</td></tr>');
    
      IF x_return_status = fnd_api.g_ret_sts_unexp_error THEN
        RAISE fnd_api.g_exc_unexpected_error;
      END IF;
    END LOOP;
  
    output('</table>');
    output('</body>');
    output('</html>');
  
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      retcode := '1';
    WHEN fnd_api.g_exc_unexpected_error THEN
      retcode := '2';
    WHEN OTHERS THEN
      retcode := '2';
      log('����� ' || g_pkg_name || ' ���� ' || l_api_name || ' �г��ִ��� ' || SQLERRM);
      errbuf := SQLERRM;
  END conc_reversal;

END cux_gl_fin_expenses_pkg;
/
