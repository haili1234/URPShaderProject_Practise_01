using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class UpdateMaterials : MonoBehaviour
{
    public Renderer[] targeRenderSet;
    public List<Material> _tempDissolveSet;
    private float threshod = 0;
    public float dissolveSpeed = 0.02f;
   
    // Start is called before the first frame update
    void Start()
    {
        targeRenderSet = GetComponentsInChildren<Renderer>();
        UpdateMaterialAction();
    }

    /// <summary>
    /// 遍历所有材质
    /// </summary>
    private void UpdateMaterialAction()
    {
        if (targeRenderSet != null)
        {
            for (int i = 0; i < targeRenderSet.Length; i++)
            {
                
                Debug.Log($"Render:{targeRenderSet[i]}");
                Material[] temp =  targeRenderSet[i].materials;
                if (temp.Length>0)
                {
                    for (int j = 0; j < temp.Length; j++)
                    {
                        _tempDissolveSet.Add(temp[j]);
                    }
                }
            }
        }
    }

    /// <summary>
    /// 更新threshod 值，达到隐身
    /// </summary>
    /// <param name="threshod"></param>
    void UpdateThreshodValue(float threshod)
    {
        if (_tempDissolveSet.Count > 0)
        {
            for (int i = 0; i < _tempDissolveSet.Count; i++)
            {
                _tempDissolveSet[i].SetFloat("_threshold",threshod);
            }
        }
    }
    
    // Update is called once per frame
    void Update()
    {
       
        if (Input.GetKey(KeyCode.A))
        {
            threshod = threshod + dissolveSpeed;
           
            if (threshod <= 1)
            {
                UpdateThreshodValue(threshod);
            }
        }
        
        if (Input.GetKey(KeyCode.B))
        {
            threshod = threshod - dissolveSpeed;
            if (threshod >= 0)
            {
                UpdateThreshodValue(threshod);
            }
        }
    }
}
